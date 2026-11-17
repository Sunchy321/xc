// expect: false
// expect: true
func check(d: int?[string]) { print(d["k"] == nil); }
check(["k": 1]);
check(["k": nil]);
