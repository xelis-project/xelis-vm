use std::{fs, path::Path, collections::HashSet};

pub struct Flattener {
    imported_by: HashSet<String>,
    imported_sources: HashSet<String>,
}

impl Flattener {
    pub fn new() -> Self {
        Self {
            imported_by: HashSet::new(),
            imported_sources: HashSet::new(),
        }
    }

    pub fn flatten(&mut self, path: &str) -> Result<String, String> {
        let std_path = Path::new(path);
        let file_name = std_path.file_name().expect("No source file in path").to_string_lossy();
        self.flatten_source(path, &file_name)
    }

    fn flatten_source(&mut self, path: &str, import: &str) -> Result<String, String> {
        let source_path = Path::new(&path).parent().expect("No Path Parent").join(&import);
        let source_str = source_path.display().to_string();

        if self.imported_by.contains(&source_str) {
            return Err(format!("Circular dependency detected: {}", path));
        }

        if self.imported_sources.contains(&source_str) {
            return Ok(String::new());
        }


        let code = fs::read_to_string(&source_path).expect(&format!("Failed to read slx file: {:?}", source_path));
        self.imported_sources.insert(source_str.clone());
        self.imported_by.insert(source_str.clone());

        let mut flattened_code = String::new();
        for line in code.lines() {
            let trimmed_line = line.trim();

            if trimmed_line.starts_with("import ") && trimmed_line.contains(" as ") {
                if let Some((import_path, namespace)) = self.extract_import_as(trimmed_line) {
                    let source_path = Path::new(&path).parent().expect("No Path Parent").join(&import_path);
                    let source_str = source_path.display().to_string();
                    flattened_code.push_str(&format!("namespace {} {{\n", namespace));
                    let resolved_code = self.flatten_source(&source_str, &import_path)?;
                    flattened_code.push_str(&resolved_code);
                    flattened_code.push_str("\n}\n");
                }
            } else if trimmed_line.starts_with("import ") {
                if let Some(import_path) = self.extract_import_path(trimmed_line) {
                    let source_path = Path::new(&path).parent().expect("No Path Parent").join(&import_path);
                    let source_str = source_path.display().to_string();
                    let resolved_code = self.flatten_source(&source_str, &import_path)?;
                    flattened_code.push_str(&resolved_code);
                }
            } else {
                flattened_code.push_str(line);
                flattened_code.push('\n');
            }
        }

        self.imported_by.remove(&source_str);
        Ok(flattened_code)
    }

    fn extract_import_path(&self, line: &str) -> Option<String> {
        let parts: Vec<&str> = line.split_whitespace().collect();
        parts.get(1).map(|path| path.trim_matches('"').to_string())
    }

    fn extract_import_as(&self, line: &str) -> Option<(String, String)> {
        let parts: Vec<&str> = line.split_whitespace().collect();
        if parts.len() >= 4 {
            let import_path = parts[1].trim_matches('"').to_string();
            let namespace_with_suffix = parts[3];

            let namespace = namespace_with_suffix
                .chars()
                .take_while(|c| c.is_alphanumeric() || *c == '_')
                .collect();

            Some((import_path, namespace))
        } else {
            None
        }
    }
}
