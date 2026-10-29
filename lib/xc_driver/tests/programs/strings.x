// 拼接是 `~`；`+` 仅算术，对 string 无定义。
// expect: ab
// expect: xyz
// expect: true
let s = "a" ~ "b";
print(s);
print("x" ~ "y" ~ "z");
print(s == "ab");
