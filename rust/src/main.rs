mod day1;
mod day3;
pub use crate::day1::day1a;
pub use crate::day3::day3a;
fn main (){
    let day1=day1a::day1a_sol();
    println!("Solution to day1: {}", day1);
    day3a::day3a_sol();
}