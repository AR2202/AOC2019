pub mod day1a{
extern crate math;
use math::round;
use std::fs;
use std::str::FromStr;

    pub fn day1a_sol() {
        let filename = "../../input/day1.txt";
        let contents = fs::read_to_string(filename);
    
    
    
        let contents2= contents.unwrap();
        let thelines = contents2.lines();
        let mut fuels =0.0;
        for s in thelines {
            let x = f64::from_str(s).unwrap();
            let fuel1 = fuel(x);
            fuels = fuels + fuel1;
        
        
        

        }
        println!("Fuel needed: {}", fuels);
    }
fn fuel(n:f64) -> f64{
    
    rounded (n) - 2.0
}
fn rounded(n:f64) -> f64{
    round::floor(n/3.0,0)
}
}