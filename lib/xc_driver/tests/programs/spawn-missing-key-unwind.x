// A missing-key panic must run every defer in the callee and caller frames.
// expect: parent
// expect: callee inner
// expect: callee outer
// expect: caller cleanup
// expect: missing key: missing
func lookup() {
    defer { print("callee outer"); }
    defer { print("callee inner"); }
    let d = ["a": 1];
    print(d["missing"]);
}

let task = spawn(do () {
    defer { print("caller cleanup"); }
    lookup();
});
print("parent");
try { task.await; print("unreachable"); } catch error { print(error); }
