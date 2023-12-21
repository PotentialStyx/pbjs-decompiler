use anyhow::{format_err, Result};
use regex::Regex;
use std::collections::HashMap;
use swc_atoms::Atom;
use swc_common::sync::Lrc;
use swc_common::{FileName, SourceMap};
use swc_ecma_ast::{
    ClassMember, Decl, EsVersion, Expr, Module, ModuleDecl, ModuleItem, PropName, Stmt,
    TsEntityName, TsEnumMemberId, TsFnParam, TsKeywordTypeKind, TsLit, TsModuleDecl, TsModuleName,
    TsNamespaceBody, TsType, TsTypeElement, TsUnionOrIntersectionType,
};
use swc_ecma_parser::Syntax;
use swc_ecma_parser::{parse_file_as_module, TsConfig};

pub fn get_ast(dts: String) -> Module {
    let cm: Lrc<SourceMap> = Default::default();

    let dts_file = cm.new_source_file(FileName::Custom("api.d.ts".into()), dts);

    let mut errors = vec![];
    let module = parse_file_as_module(
        &dts_file,
        Syntax::Typescript(TsConfig {
            tsx: false,
            decorators: false,
            dts: true,
            no_early_errors: false,
            disallow_ambiguous_jsx_like: false,
        }),
        EsVersion::EsNext,
        None,
        &mut errors,
    )
    .unwrap();

    assert_eq!(errors, vec![]);
    module
}

pub fn parse(dts: String, js: String) -> Result<ParsedProto> {
    let module = get_ast(dts);
    for item in module.body {
        if let ModuleItem::ModuleDecl(ModuleDecl::ExportDecl(module)) = item {
            if let Decl::TsModule(decl) = module.decl {
                if module_name(&decl) == Some(Atom::new("replit")) {
                    let module = decl;
                    if let TsNamespaceBody::TsModuleBlock(block) = module.body.make_result()? {
                        for item in block.body {
                            if let ModuleItem::Stmt(Stmt::Decl(Decl::TsModule(decl))) = item {
                                if module_name(&decl) == Some(Atom::new("goval")) {
                                    let module = decl;
                                    if let TsNamespaceBody::TsModuleBlock(block) =
                                        module.body.make_result()?
                                    {
                                        for item in block.body {
                                            if let ModuleItem::Stmt(Stmt::Decl(Decl::TsModule(
                                                decl,
                                            ))) = item
                                            {
                                                if module_name(&decl) == Some(Atom::new("api")) {
                                                    return recursive_parse(
                                                        *decl,
                                                        &js,
                                                        "replit.goval.api".to_string(),
                                                    );
                                                }
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    }
    Err(format_err!("Couldn't find replit.goval.api namespace"))
}

pub type ParsedProto = Vec<ParsedProtoItem>;

#[derive(Debug, Clone)]
pub struct ParsedField {
    pub id: i64,
    pub name: Atom,
    pub r#type: String,
    pub required: bool,
    pub repeated: bool,
}

#[derive(Debug, Clone, Default)]
pub struct ParsedOneOf {
    pub name: Atom,
    pub fields: Vec<ParsedField>,
}

#[derive(Debug, Clone)]
pub struct ParsedMessage {
    pub name: Atom,
    pub extra: Option<ParsedProto>,
    pub fields: Vec<ParsedField>,
    pub oneofs: Vec<ParsedOneOf>,
}

#[derive(Debug, Clone)]
pub struct ParsedEnumItem {
    pub id: i64,
    pub name: Atom,
}

#[derive(Debug, Clone)]
pub enum ParsedProtoItem {
    Enum {
        name: Atom,
        fields: Vec<ParsedEnumItem>,
    },
    Message(ParsedMessage),
}

enum FieldOrRef {
    ParsedField(ParsedField),
    Ref(String),
}

pub fn recursive_parse(_decl: TsModuleDecl, js_code: &str, path: String) -> Result<ParsedProto> {
    let mut proto: ParsedProto = vec![];
    if let TsNamespaceBody::TsModuleBlock(decl) = _decl.body.make_result()? {
        for item in decl.body {
            let stmt = match item {
                ModuleItem::ModuleDecl(_) => unimplemented!(),
                ModuleItem::Stmt(stmt) => stmt,
            };

            let decl = match stmt {
                Stmt::Decl(decl) => decl,
                _ => {
                    unimplemented!()
                }
            };

            match decl {
                Decl::TsEnum(value) => {
                    let mut members = vec![];
                    for member in value.members {
                        let value = match *member.init.make_result()? {
                            Expr::Lit(literal) => match literal {
                                swc_ecma_ast::Lit::Num(num) => num.value as i64,
                                _ => todo!("{literal:#?}"),
                            },
                            _ => unimplemented!(),
                        };
                        members.push(ParsedEnumItem {
                            id: value,
                            name: member.id.get_id_atom().clone(),
                        });
                    }

                    proto.push(ParsedProtoItem::Enum {
                        name: value.id.sym,
                        fields: members,
                    });
                }
                Decl::TsModule(module) => {
                    let wanted = module.id.get_id_atom();
                    let matches = proto
                        .iter()
                        .enumerate()
                        .filter(|(_, r)| {
                            if let ParsedProtoItem::Message(msg) = r {
                                return msg.name == *wanted;
                            };
                            false
                        })
                        .map(|(index, _)| index)
                        .collect::<Vec<_>>();
                    assert!(matches.len() <= 1);
                    let message = if matches.len() == 1 {
                        proto.get_mut(matches[0]).unwrap()
                    } else {
                        let message = ParsedProtoItem::Message(ParsedMessage {
                            name: wanted.clone(),
                            extra: None,
                            fields: vec![],
                            oneofs: vec![],
                        });
                        proto.push(message);
                        let len = proto.len();
                        proto.get_mut(len - 1).unwrap()
                    };

                    let path = format!("{}.{}", path.clone(), wanted);

                    if let ParsedProtoItem::Message(message) = message {
                        message.extra = Some(recursive_parse(*module, js_code, path)?);
                    } else {
                        unimplemented!()
                    }
                }
                Decl::Class(class) => {
                    let wanted = class.ident.sym;
                    let matches = proto
                        .iter()
                        .enumerate()
                        .filter(|(_, r)| {
                            if let ParsedProtoItem::Message(msg) = r {
                                return msg.name == *wanted;
                            };
                            false
                        })
                        .map(|(index, _)| index)
                        .collect::<Vec<_>>();
                    assert!(matches.len() <= 1);
                    let message = if matches.len() == 1 {
                        proto.get_mut(matches[0]).unwrap()
                    } else {
                        let message = ParsedProtoItem::Message(ParsedMessage {
                            name: wanted.clone(),
                            extra: None,
                            fields: vec![],
                            oneofs: vec![],
                        });
                        proto.push(message);
                        let len = proto.len();
                        proto.get_mut(len - 1).unwrap()
                    };

                    let message = if let ParsedProtoItem::Message(message) = message {
                        message
                    } else {
                        unimplemented!()
                    };

                    let mut oneof_fields: HashMap<String, ParsedField> = HashMap::new();
                    let mut fields: Vec<FieldOrRef> = vec![];
                    'mainloop: for _prop in class.class.body {
                        let prop = match _prop {
                            ClassMember::ClassProp(prop) => prop,
                            _ => continue,
                        };

                        match *prop.clone().type_ann.make_result()?.type_ann {
                            TsType::TsKeywordType(keyword) => {
                                let name = prop.key.get_id_atom().to_string();
                                let (id, type_name) = find_id(
                                    js_code,
                                    format!("{path}.{wanted}"),
                                    name,
                                    false,
                                    type_kind_to_string(keyword.kind)?,
                                )?;
                                fields.push(FieldOrRef::ParsedField(ParsedField {
                                    id,
                                    name: prop.key.get_id_atom().clone(),
                                    r#type: type_name,
                                    required: true,
                                    repeated: false,
                                }));
                            }
                            TsType::TsTypeRef(reference) => {
                                let name = prop.key.get_id_atom().to_string();
                                let (id, type_name) = find_id(
                                    js_code,
                                    format!("{path}.{wanted}"),
                                    name,
                                    false,
                                    reconstruct_struct_name(reference.type_name)?,
                                )?;
                                fields.push(FieldOrRef::ParsedField(ParsedField {
                                    id,
                                    name: prop.key.get_id_atom().clone(),
                                    r#type: type_name,
                                    required: true,
                                    repeated: false,
                                }));
                            }
                            TsType::TsArrayType(array) => {
                                let type_name = match *array.elem_type {
                                    TsType::TsKeywordType(keyword) => {
                                        type_kind_to_string(keyword.kind)?
                                    }
                                    TsType::TsTypeRef(reference) => {
                                        reconstruct_struct_name(reference.type_name)?
                                    }
                                    _ => unimplemented!(),
                                };
                                let name = prop.key.get_id_atom().to_string();
                                let (id, type_name) = find_id(
                                    js_code,
                                    format!("{path}.{wanted}"),
                                    name,
                                    true,
                                    type_name,
                                )?;
                                fields.push(FieldOrRef::ParsedField(ParsedField {
                                    id,
                                    name: prop.key.get_id_atom().clone(),
                                    r#type: type_name,
                                    required: true,
                                    repeated: true,
                                }));
                            }
                            TsType::TsUnionOrIntersectionType(_union_type) => {
                                let union_type = match _union_type {
                                    TsUnionOrIntersectionType::TsIntersectionType(_) => {
                                        unimplemented!()
                                    }
                                    TsUnionOrIntersectionType::TsUnionType(union_type) => {
                                        union_type
                                    }
                                };
                                let mut oneof = ParsedOneOf {
                                    name: prop.key.get_id_atom().clone(),
                                    ..ParsedOneOf::default()
                                };

                                if union_type.types.len() == 2
                                    && ts_type_to_string(*union_type.types.get(1).unwrap().clone())?
                                        == "null"
                                {
                                    let type_name = ts_type_to_string(
                                        *union_type.types.get(0).unwrap().clone(),
                                    )?;
                                    let name = prop.key.get_id_atom().clone();
                                    let name2 = prop.key.get_id_atom().to_string();
                                    let (id, type_name) = find_id(
                                        js_code,
                                        format!("{path}.{wanted}"),
                                        name2,
                                        false,
                                        type_name,
                                    )?;
                                    fields.push(FieldOrRef::Ref(name.to_string()));
                                    oneof_fields.insert(
                                        name.to_string(),
                                        ParsedField {
                                            id,
                                            name,
                                            r#type: type_name,
                                            required: true,
                                            repeated: false, // TODO: repeated fields inside oneof?
                                        },
                                    );
                                    continue 'mainloop;
                                }

                                for field_type in union_type.types {
                                    let type_name = ts_type_to_string(*field_type)?;

                                    if &type_name == "null" {
                                        unimplemented!();
                                    }

                                    oneof.fields.push(oneof_fields.remove(&type_name).unwrap());
                                }

                                message.oneofs.push(oneof);
                            }
                            TsType::TsLitType(literal) => {
                                message.oneofs.push(ParsedOneOf {
                                    name: prop.key.get_id_atom().clone(),
                                    fields: vec![oneof_fields
                                        .remove(&ts_lit_to_string(literal.lit)?)
                                        .unwrap()],
                                });
                            }
                            TsType::TsTypeLit(literal) => {
                                let signature = match &literal.members[0] {
                                    TsTypeElement::TsIndexSignature(sig) => sig.clone(),
                                    _ => unimplemented!(),
                                };

                                let first = match signature.params[0].clone() {
                                    TsFnParam::Ident(ident) => {
                                        ts_type_to_string(*ident.type_ann.make_result()?.type_ann)?
                                    }
                                    _ => unimplemented!(),
                                };

                                let second =
                                    ts_type_to_string(*signature.type_ann.make_result()?.type_ann)?;

                                let name = prop.key.get_id_atom().to_string();
                                fields.push(FieldOrRef::ParsedField(ParsedField {
                                    id: find_id(
                                        js_code,
                                        format!("{path}.{wanted}"),
                                        name,
                                        false,
                                        String::new(),
                                    )?
                                    .0,
                                    name: prop.key.get_id_atom().clone(),
                                    r#type: format!("map<{first},{second}>"),
                                    required: true,
                                    repeated: false,
                                }));
                            }
                            _ => {
                                eprintln!("Unknown type: {prop:#?}");
                            }
                        }
                    }
                    for field in fields {
                        match field {
                            FieldOrRef::ParsedField(parsed) => message.fields.push(parsed),
                            FieldOrRef::Ref(ident) => {
                                match oneof_fields.get(&ident) {
                                    None => {}
                                    Some(field) => message.fields.push(field.clone()),
                                };
                            }
                        }
                    }
                }
                _ => {}
            }
        }
    }

    Ok(proto)
}

pub fn find_id(
    mut js: &str,
    fqn: String,
    field: String,
    repeated: bool,
    mut rtype: String,
) -> Result<(i64, String)> {
    static RE: once_cell::sync::Lazy<Regex> =
        once_cell::sync::Lazy::new(|| Regex::new(r"case ([0-9]+):").unwrap());
    static RE2: once_cell::sync::Lazy<Regex> = once_cell::sync::Lazy::new(|| {
        Regex::new(r"reader\.([a-z]?int[0-9]{0,2}|double)\(\)").unwrap()
    });
    let idx = js
        .find(&format!(
            " message. Does not implicitly {{@link {fqn}.verify|verify}} messages."
        ))
        .unwrap();
    js = &js[idx..];
    let idx = js
        .find(&format!("message.{field} ="))
        .unwrap_or_else(|| js.find(&format!("message[\"{field}\"] =")).unwrap());
    let js2 = &js[..idx];
    js = &js[idx..];
    let cases: Vec<_> = RE.captures_iter(js2).collect();
    let case_capture = &cases[cases.len() - 1];
    let id: i64 = case_capture.get(1).unwrap().as_str().parse()?;

    if repeated {
        let idx = js.find(&format!(".{field}.push(")).unwrap();
        js = &js[idx..];
    }

    if rtype == "number" {
        rtype = RE2
            .captures(js)
            .unwrap()
            .get(1)
            .unwrap()
            .as_str()
            .to_owned();
    }

    Ok((id, rtype))
}

pub fn ts_type_to_string(ty: TsType) -> Result<String> {
    Ok(match ty {
        TsType::TsKeywordType(keyword) => type_kind_to_string(keyword.kind)?,
        TsType::TsTypeRef(reference) => reconstruct_struct_name(reference.type_name)?,
        TsType::TsLitType(literal) => ts_lit_to_string(literal.lit)?,
        _ => unimplemented!(),
    })
}

pub fn ts_lit_to_string(lit: TsLit) -> Result<String> {
    Ok(match lit {
        TsLit::Str(lit) => lit.value.to_string(),
        TsLit::Number(lit) => lit.raw.make_result()?.to_string(),
        TsLit::BigInt(lit) => lit.raw.make_result()?.to_string(),
        TsLit::Bool(lit) => lit.value.to_string(),
        TsLit::Tpl(_) => unimplemented!(),
    })
}

pub fn reconstruct_struct_name(_name: TsEntityName) -> Result<String> {
    let name = match _name {
        TsEntityName::Ident(ident) => return Ok(ident.sym.to_string()),
        TsEntityName::TsQualifiedName(name) => name,
    };

    let result = match name.left {
        TsEntityName::Ident(ident) => ident.sym.to_string(),
        TsEntityName::TsQualifiedName(_) => reconstruct_struct_name(name.left)?,
    } + "."
        + name.right.as_ref();

    Ok(result)
}

pub fn module_name(decl: &TsModuleDecl) -> Option<Atom> {
    if let TsModuleName::Ident(ident) = &decl.id {
        return Some(ident.sym.clone());
    }
    None
}

pub fn type_kind_to_string(type_kind: TsKeywordTypeKind) -> Result<String> {
    let type_str = serde_json::to_string(&type_kind)?;
    Ok(type_str[1..type_str.len() - 1].to_string())
}

trait GetIdAtom<'a> {
    fn get_id_atom(&'a self) -> &'a Atom;
}

impl<'a> GetIdAtom<'a> for TsEnumMemberId {
    fn get_id_atom(&'a self) -> &'a Atom {
        match self {
            TsEnumMemberId::Ident(ident) => &ident.sym,
            TsEnumMemberId::Str(str) => &str.value,
        }
    }
}

impl<'a> GetIdAtom<'a> for TsModuleName {
    fn get_id_atom(&'a self) -> &'a Atom {
        match self {
            TsModuleName::Ident(ident) => &ident.sym,
            TsModuleName::Str(str) => &str.value,
        }
    }
}

impl<'a> GetIdAtom<'a> for PropName {
    fn get_id_atom(&'a self) -> &'a Atom {
        match self {
            PropName::Ident(ident) => &ident.sym,
            PropName::Str(str) => &str.value,
            // PropName::Num(num) => &num.raw.unwrap(),
            // PropName::BigInt(num) => &num.raw.unwrap(),
            _ => unimplemented!(),
        }
    }
}

trait MakeResult<T> {
    fn make_result(self) -> Result<T>;
}

impl<T> MakeResult<T> for Option<T> {
    fn make_result(self) -> Result<T> {
        match self {
            None => Err(format_err!("None type found")),
            Some(val) => Ok(val),
        }
    }
}
