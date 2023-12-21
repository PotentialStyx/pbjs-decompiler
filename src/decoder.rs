use crate::{parser, reconstruct};
use anyhow::Result;

pub fn decode(dts: String, js: String, version: String) -> Result<String> {
    let parsed = parser::parse(dts, js)?;
    reconstruct::reconstruct(parsed, &version)
}
