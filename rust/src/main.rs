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
    let day4Part1 = day4ab::day4a_sol();
    let day4Part2 = day4ab::day4b_sol();
    println!("Solution to day4 part 1: {}",day4Part1);
    println!("Solution to day4 part 2: {}",day4Part2)

}