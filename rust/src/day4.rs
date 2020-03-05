pub mod day4ab{

    pub fn day4a_sol()-> usize{
        let totalRange : Vec <i32> = (356261..846303).collect();
        let diglists:Vec<&i32>= totalRange.iter()
        .filter(|y|digitized_contains_multiples(**y))
        .filter(|y|digitized_monotonic_increasing(**y))
        .collect();
        
       
        let possibilities=diglists.len();
        possibilities 

        

    }
    pub fn day4b_sol()-> usize{
        let totalRange : Vec <i32> = (356261..846303).collect();
        let diglists:Vec<&i32>= totalRange.iter()
        .filter(|y|digitized_contains_doubles(**y))
        .filter(|y|digitized_monotonic_increasing(**y))
        .collect();
        
       
        let possibilities=diglists.len();
        possibilities 

        

    }
    fn to_digits(num:i32)->Vec<i32> {
        let mut x =num;
        let mut diglist:Vec<i32> = Vec::new();
        if x<0 {
            panic!("Number is negative");
        }
        else if x == 0 {
            diglist.push(0);

        }
        else {
        while x>0 {
            diglist.push(x%10);
            x/=10;
        }
    }
        diglist.reverse();
        diglist

    }
    fn contains_multiples(slice:Vec<i32>)-> bool {
        (1..slice.len()).any(|i| slice[i..].contains(&slice[i - 1]))
    }
    fn digitized_contains_multiples(num:i32)-> bool {
        let slice = to_digits(num);
        contains_multiples(slice)
    }
    fn monotonic_increasing(slice:Vec<i32>)->bool{
        (1..slice.len()).all(|i| &slice[i]>=&slice[i - 1])
    }
    fn digitized_monotonic_increasing(num:i32)-> bool {
        let slice = to_digits(num);
        monotonic_increasing(slice)
    }
//note: this function will return true iff exactly 2 consecutive numbers are equal. The condition that the duplicate numbers be consecutive is fulfilled by all numbers whose digits are also monotonically increasing, therefore this condition is sufficient

    fn contains_doubles(slice:Vec<i32>)->bool{
        ((2..(slice.len()-1)).any(|i| &slice[i]==&slice[i - 1] && &slice[i]!=&slice[i-2] && &slice[i] != &slice[i+1])) 
        ||( &slice[1]==&slice[0] && &slice[1]!=&slice[2])
        ||(&slice[slice.len()-1]==&slice[slice.len()-2] && &slice[slice.len()-2]!=&slice[slice.len()-3])
    }
    fn digitized_contains_doubles(num:i32)-> bool {
        let slice = to_digits(num);
        contains_doubles(slice)
    }
    
#[cfg(test)]
mod tests {
    use super::*;
    
    
    #[test]
    fn to_digits_test() {
        
        assert_eq!(to_digits(10) , vec![1,0]);
        assert_eq!(to_digits(2),vec![2]);
        assert_eq!(to_digits(0),vec![0]);
        assert_eq!(to_digits(123),vec![1,2,3]);
    }
    #[test]
    #[should_panic]
    fn to_digits_negative() {
        to_digits(-2);
    }
    
    #[test]
    fn contains_multiples_test() {
        
        assert!(contains_multiples(vec![1,2,2,3]));
        assert!(!contains_multiples(vec![1,2,3]));
        assert!(contains_multiples(vec![1,1,1,3]));
        assert!(contains_multiples(vec![1,2,3,3,4,4]));
        assert!(contains_multiples(vec![1,2,1,3]));
        
    } 
    #[test]
    fn digitized_contains_multiples_test() {
        
        assert!(digitized_contains_multiples(12344));
        assert!(digitized_contains_multiples(11344));
        assert!(digitized_contains_multiples(12314));
        assert!(!digitized_contains_multiples(1234));
        
        
    } 
    #[test]
    fn digitized_contains_doubles_test() {
        
        assert!(digitized_contains_doubles(12344));
        assert!(digitized_contains_doubles(11344));
        assert!(digitized_contains_doubles(11133444));
        assert!(!digitized_contains_doubles(123334));
        assert!(!digitized_contains_doubles(1234));
        
        
    } 
    #[test]
    fn filter_digitized_contains_multiples_test() {
        let one :i32 = 121;
        let two:i32 = 122;
        let numbers:Vec<i32>=(121..123).collect();
        let filtered:Vec<&i32>=numbers.iter()
        .filter(|x|digitized_contains_multiples(**x)).collect();
        assert_eq!(filtered,vec![&one,&two]);
        
        
        
    } 
    #[test]
    fn len_test(){
        let numbers:Vec<i32>=(121..123).collect();
        let filtered:Vec<&i32>=numbers.iter()
        .filter(|x|digitized_contains_multiples(**x)).collect();
        let length=filtered.len();
        assert_eq!(length,2);

    }
    #[test]
    fn monotonic_increasing_test() {
        
        assert!(monotonic_increasing(vec![1,2,2,3]));
        assert!(monotonic_increasing(vec![1,2,3]));
        assert!(monotonic_increasing(vec![1,1,1,3]));
        assert!(monotonic_increasing(vec![1,2,3,3,4,4]));
        assert!(!monotonic_increasing(vec![1,2,1,3]));
        assert!(!monotonic_increasing(vec![3,2,1,3]));
        
    }  
    #[test]
    fn day4a_sol_test() {
        
        assert_eq!(day4a_sol() , 544);
    
    }
    #[test]
    fn day4b_sol_test() {
        
        assert_eq!(day4b_sol() , 334);
    
    }
}
}