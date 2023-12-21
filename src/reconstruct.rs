use crate::parser::{ParsedField, ParsedProto, ParsedProtoItem};
use anyhow::{format_err, Result};

pub fn reconstruct(proto: ParsedProto, version: &str) -> Result<String> {
    let res: &str = &recursive_reconstruct(proto, String::new())?;
    Ok(format!(
        "// @replit/protocol {version}
syntax = \"proto3\";

package api;

import \"google/protobuf/timestamp.proto\";
import \"google/protobuf/struct.proto\";
import \"google/protobuf/field_mask.proto\";
import \"google/protobuf/empty.proto\";

"
    ) + res)
}

fn recursive_reconstruct(proto: ParsedProto, tabs: String) -> Result<String> {
    let mut start = String::new();
    for item in proto {
        match item {
            ParsedProtoItem::Enum { name, fields } => {
                let mut res = format!("{tabs}enum {name} {{\n");
                for field in fields {
                    res += &format!("{tabs}\t{} = {};\n", field.name, field.id);
                }
                res += &format!("{tabs}}}\n\n");

                start += &res;
            }
            ParsedProtoItem::Message(message) => {
                let mut res = format!("{tabs}message {} {{\n", message.name);

                if let Some(extra) = message.extra {
                    res += &recursive_reconstruct(extra.clone(), format!("{tabs}\t"))?;
                }

                for field in message.fields {
                    res += &field_to_string(field, &tabs)?;
                }

                for oneof in message.oneofs {
                    res += &format!("{tabs}\toneof {} {{\n", oneof.name);

                    for field in oneof.fields {
                        res += &field_to_string(field, &(tabs.clone() + "\t"))?;
                    }

                    res += &format!("{tabs}\t}}\n");
                }

                res += &format!("{tabs}}}\n\n");

                start += &res;
            }
        }
    }
    Ok(start)
}

fn field_to_string(mut field: ParsedField, tabs: &str) -> Result<String> {
    let matcher: &str = &field.r#type;
    match matcher {
        "number" => {
            return Err(format_err!(
                "Got type `number` for field {}. Please open an issue on github.",
                field.name
            ))
        }
        "Uint8Array" => field.r#type = "bytes".to_owned(),
        "boolean" => field.r#type = "bool".to_owned(),
        _ => {}
    }

    field.r#type = field.r#type.replace("replit.goval.api.", "");

    let extra = if field.repeated { "repeated " } else { "" };

    Ok(format!(
        "{tabs}\t{extra}{} {} = {};\n",
        field.r#type, field.name, field.id
    ))
}
