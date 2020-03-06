mod day1;
mod day3;
mod day4;
pub use crate::day1::day1a;
pub use crate::day3::day3a;
pub use crate::day4::day4ab;
fn main (){
    let day1=day1a::day1a_sol();
    println!("Solution to day1: {}", day1);
    //day3a::day3a_sol();
    
    let day4a=day4ab::day4_sol(day4ab::digitized_contains_multiples);
    let day4b=day4ab::day4_sol(day4ab::digitized_contains_doubles);
    println!("Solution to day4 part 1: {}",day4a);
    println!("Solution to day4 part 2: {}",day4b);


}