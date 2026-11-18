// error: borrowed
func take(values: int[]) {}
func bad() {
    let values = [1];
    let r = &values;
    take(values);
    print((*r)[0]);
}
bad();
