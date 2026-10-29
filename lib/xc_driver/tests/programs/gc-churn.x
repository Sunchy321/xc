// 大量短命数组/字符串/闭包：在 XVM_GC_EVERY_ALLOC=1 压力下也必须输出正确。
// expect: 5150
// expect: 49
let mut i = 0;
let mut total = 0;
while i < 100 {
    let a = [i, i + 1, i + 2];
    let s = "x" ~ "y";
    total = total + a[2];
    i = i + 1;
}
print(total);
let mut f = do (x: int) { x };
let mut j = 0;
while j < 50 {
    let k = j;
    f = do (x: int) { x + k };
    j = j + 1;
}
print(f(0));
