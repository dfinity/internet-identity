// TODO
#[test]
fn can_store_and_retrieve_origin() {
    todo!()
}

#[test]
fn should_add_application_correctly() {
    let memory = VectorMemory::default();
    let mut storage = Storage::new((10_000, 3_784_873), memory);

    let origin: FrontendHostname = "https://example.com".to_string();

    let number = storage.lookup_or_insert_application_with_origin(&origin);

    let application = storage.stable_application_memory.get(&number).unwrap();

    assert_eq!(origin, application.origin);
}
