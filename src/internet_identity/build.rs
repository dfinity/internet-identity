use sha2::Digest;
use std::env;
use std::fs;
use std::io::Write;
use std::path::Path;

#[derive(Debug)]
pub enum ContentEncoding {
    Identity,
    GZip,
}

fn hash_file(path: &str) -> [u8; 32] {
    let bytes = fs::read(path).unwrap_or_else(|e| panic!("failed to read file {}: {}", path, e));
    let mut hasher = sha2::Sha256::new();
    hasher.update(&bytes);
    hasher.finalize().into()
}

fn main() -> Result<(), String> {
    let out_dir = env::var_os("OUT_DIR").unwrap();
    let assets_module_path = Path::new(&out_dir).join("assets.rs");
    let asset_rel_paths = [
        ("/", "../../dist/index.html", ContentEncoding::Identity),

        // The FAQ page is the same webapp, but the webapp routes to the correct page
        // (well _almost_, the faq includes tailwind)
        ("/faq", "../../dist/faq/index.html", ContentEncoding::Identity),
        (
            "/index.html",
            "../../dist/index.html",
            ContentEncoding::Identity,
        ),
        ("/index.js", "../../dist/index.js.gz", ContentEncoding::GZip),
        (
            "/loader.webp",
            "../../dist/loader.webp",
            ContentEncoding::Identity,
        ),
        (
            "/favicon.ico",
            "../../dist/favicon.ico",
            ContentEncoding::Identity,
        ),
    ];

    for (_, path, _) in asset_rel_paths.iter() {
        if !Path::new(path).exists() {
            return Err(format!("asset file {} doesn't exist", path));
        }
    }

    let mut assets_module = fs::File::create(&assets_module_path).map_err(|e| {
        format!(
            "failed to create file {}: {}",
            assets_module_path.display(),
            e
        )
    })?;
    writeln!(
        assets_module,
        r#"
#[derive(Debug, PartialEq, Eq)]
pub enum ContentEncoding {{
    Identity,
    GZip,
}}
pub fn for_each_asset(mut f: impl FnMut(&'static str, ContentEncoding, &'static [u8], &[u8; 32])) {{
"#
    )
    .unwrap();

    for (name, path, encoding) in asset_rel_paths.iter() {
        let hash = hash_file(path);
        let abs_path = Path::new(path).canonicalize().unwrap();
        writeln!(
            assets_module,
            "  f(\"{}\", ContentEncoding::{:?}, &include_bytes!(\"{}\")[..], &{:?});",
            name,
            encoding,
            abs_path.display(),
            hash
        )
        .unwrap();
    }
    writeln!(assets_module, "}}").unwrap();

    println!("cargo:rerun-if-changed=build.rs");
    for (_, path, _) in asset_rel_paths.iter() {
        println!("cargo:rerun-if-changed={}", path);
    }

    Ok(())
}
