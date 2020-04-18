pub mod day10a{
    extern crate ndarray;
    use ndarray::arr1;
    extern crate nalgebra as na;
    extern crate nalgebra_glm as glm;
    extern crate itertools;
    extern crate num;
    extern crate array_tool;
    use std::fs;
    use std::str::FromStr;
    use nalgebra_glm::are_collinear2d;
    use nalgebra_glm::vec2;
    use nalgebra_glm::TVec2;
    use nalgebra_glm::normalize;
    use itertools::Itertools;
    use num::integer::gcd;
    use num::integer::div_floor;
    use array_tool::vec::Uniq;
    
    pub fn day10a_sol(){
        
        let filename = "../input/day10.txt";
        let contents = fs::read_to_string(filename);
    
    
    
        let contents2= contents.unwrap();
        let thelines = contents2.lines();
        
        let mut vector :Vec<TVec2<i32>>= Vec::new();
        let mut yval=0;

        for l in thelines {
            let mut xval=0;
            for c in l.chars(){
                let coord = vec2(xval,yval);
                if c =='#'{
                    vector.push(coord);
                }
                xval=xval+1;

            }
            yval=yval+1;
            
        
        }
        let sol10=max_detectable(vector);
        println!("sol10 {}",sol10);
       
    }
    pub fn example1()->(){
        let a1 = vec2(1,0);
        let a2=vec2(4,0);
        let a3=vec2(0,2);
        let a4=vec2(1,2);
        let a5=vec2(2,2);
        let a6=vec2(3,2);
        let a7=vec2(4,2);
        let a8=vec2(4,3);
        let a9=vec2(3,4);
        let a10=vec2(4,4);
        let asteroidlist = vec![a1,a2,a3,a4,a5,a6,a7,a8,a9,a10];
        
        
        let maxl1=max_detectable(asteroidlist);
        println!("length alldiffer {}",maxl1);
       

    }
    fn max_detectable(asteroidlist:Vec<TVec2<i32>>)->usize{
        let alldiffer:Vec<Vec<TVec2<i32>>>=
        asteroidlist
        .iter()
        .map(|a|difference_to_all(a,&asteroidlist))
        .map(|list|list.unique())
        .collect();
        let len_alldiffer:Vec<usize>=
        alldiffer
        .iter()
        .map(|list|list.len())
        .collect();
        let maxlen=len_alldiffer.iter().max();
        let maxl=match maxlen{
            Some(i)=>*i,
            None=>0,
        };
        return maxl
    }
    fn difference_to_all(vect:&TVec2<i32>,pointlist: &Vec<TVec2<i32>>)->Vec<TVec2<i32>>{
        let mut pointlistclone=pointlist.clone();
        pointlistclone.retain(|&v| v != *vect);
        let differences:Vec<TVec2<i32>> = pointlistclone
        .iter()
        .map(|v|*v-vect)
        .collect();
        let norm_differences:Vec<TVec2<i32>>=
        differences
        .iter()
        .map(|v|reduced_vec(v))
        .collect();
        norm_differences
    }
    fn reduced_vec(vect:&TVec2<i32>)->TVec2<i32>{
        let gcdiv=gcd(vect[0],vect[1]);
        let newvec=match gcdiv {
            0 => *vect,
            _=> vec2(div_floor(vect[0],gcdiv),div_floor(vect[1],(gcdiv))),
        };
        return newvec
        
    }
}