// place 引用与赋值可见（备忘录附录 A 范例二）。
// expect: 2
// expect: 3
let mut x = 1;
let r = &mut x;
*r = 2;
let a = [1, 2, 3];
let p = &a[1];
x = 3;
print(*p);
print(*r);
