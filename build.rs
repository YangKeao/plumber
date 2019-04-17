extern crate build_deps;

fn main() {
    build_deps::rerun_if_changed_paths("grammar/*").unwrap();
}
