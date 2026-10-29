// try/catch：同帧 throw 直跳；跨帧经 er 传播（JMPERR 消费）。
// expect: 5
// expect: 0
// expect: 10
func f(n: int) { if n > 0 { throw n; } return 0; }
try f(5) catch e { print(e); }
print(try f(0) catch e { -1 });
let v = try f(5) catch e { e * 2 };
print(v);
