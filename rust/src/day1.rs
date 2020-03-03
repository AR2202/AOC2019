pub mod day1a{
extern crate math;
use math::round;
use std::fs;
use std::str::FromStr;

    pub fn day1a_sol() -> f64{
        let filename = "../input/day1.txt";
        let contents = fs::read_to_string(filename);
    
    
    
        let contents2= contents.unwrap();
        let thelines = contents2.lines();
        let mut fuels =0.0;
        for s in thelines {
            let x = f64::from_str(s).unwrap();
            let fuel1 = fuel(x);
            fuels = fuels + fuel1;
        
        
        

        }
        //println!("Fuel needed: {}", fuels);
        return fuels
    }
fn fuel(n:f64) -> f64{
    
    rounded (n) - 2.0
}
fn rounded(n:f64) -> f64{
    round::floor(n/3.0,0)
}

// Test for day1 part1
#[cfg(test)]
mod tests {
    use super::*;
    
    
    #[test]
    fn fuel_result() {
        
        assert_eq!(fuel(12.0) , 2.0);
    }
    
    #[test]
    fn rounded_result(){
        assert_eq!(rounded(12.0),4.0);
    }
    #[test]
    fn day1a_result(){
        assert_eq!(day1a_sol(),3369286.0);
    }
}
}