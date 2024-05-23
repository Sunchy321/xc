fn main() {
    let v = vec![1,3,5,7,2,4,6,8];

    let r = v.into_iter().try_fold(0, |p, i| if i > 5 { Ok(i) } else { Err(i) });

    println!("{}", r);
}