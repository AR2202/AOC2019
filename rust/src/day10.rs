pub mod day10a{
    extern crate ndarray;
    extern crate nalgebra as na;
    extern crate nalgebra_glm as glm;
    extern crate itertools;
    extern crate num;
    extern crate array_tool;
    use std::fs;
    use nalgebra_glm::vec2;
    use nalgebra_glm::TVec2;
    use num::integer::gcd;
    use num::integer::div_floor;
    use array_tool::vec::Uniq;
    
    pub fn day10a_b_sol(){
        
        let filename = "../input/day10.txt";
        let contents = fs::read_to_string(filename);
    
    
    
        let contents2 = contents.unwrap();
        let thelines = contents2.lines();
        
        let mut vector :Vec<TVec2<i32>>= Vec::new();
        let mut yval=0;

        for l in thelines {
            let mut xval = 0;
            for c in l.chars(){
                let coord = vec2(xval,yval);
                if c == '#'{
                    vector.push(coord);
                }
                xval = xval+1;

            }
            yval=yval+1;
            
        
        }
        let vectorcopy=vector.clone();
        let sol10a = max_detectable(vector);
        let sol10b = find_asteroid200(vectorcopy);
        println!("sol10a {}",sol10a);
        println!("sol10b {}",sol10b);
       
    }
    pub fn example1()->usize{
        let a1 = vec2(1,0);
        let a2 = vec2(4,0);
        let a3 = vec2(0,2);
        let a4 = vec2(1,2);
        let a5 = vec2(2,2);
        let a6 = vec2(3,2);
        let a7 = vec2(4,2);
        let a8 = vec2(4,3);
        let a9=vec2(3,4);
        let a10=vec2(4,4);
        let asteroidlist = vec![a1,a2,a3,a4,a5,a6,a7,a8,a9,a10];
        
        let maxl1=max_detectable(asteroidlist);
        println!("example1 max detectable{}",maxl1);
        return maxl1

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
    //This function only works because I already know from part 1 that 
    //more than 200 asteroids are detectable
    fn find_asteroid200(asteroidlist:Vec<TVec2<i32>>)->i32{
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
        let maxpos= len_alldiffer.iter().position(|&x| x == maxl);
        let maxp=match maxpos{
            Some(i)=>i,
            None=>0,
        };
        let vectors= &alldiffer[maxp];
        let asteroidcoord=&asteroidlist[maxp];
        println!("Asteroid with detecting station: {}",asteroidcoord);
        let mut positives:Vec<&TVec2<i32>>=
        vectors
        .iter()
        .filter(|a|a[0]>=0)
        .collect();
        let mut negatives:Vec<&TVec2<i32>>=
        vectors
        .iter()
        .filter(|a|a[0]<0)
        .collect();
        
        let lenpos=positives.len();

        negatives
            .sort_by(|a, b| (a[1]*b[0])
            .partial_cmp(&(b[1]*a[0]))
            .unwrap());
        positives
            .sort_by(|a,b| a[1]
            .partial_cmp(&b[1])
            .unwrap());
        let index = 200-1-lenpos;
        let coord200 = if lenpos < 200{
            negatives[index]+asteroidcoord
         
        }
        else {
            *positives[199]
        };
        println!("coordinate of asteroid 200: {}",coord200);
        let resultcode = 100*coord200[0]+coord200[1];
        
        return resultcode
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
            _=> vec2(div_floor(vect[0], gcdiv),div_floor(vect[1], gcdiv)),
        };
        return newvec
        
    }
    #[cfg(test)]
mod tests {
    use super::*;
    
    
    #[test]

    fn reduced_vec_test() {
        let vec1 = TVec2::new(2,4);
        let vec2 = TVec2::new(3,6);
        let vec3 = TVec2::new(3,-6);
        let vec_red = TVec2::new(1,2);
        assert_eq!(reduced_vec(&vec1), *&vec_red);
        assert_eq!(reduced_vec(&vec2), *&vec_red);
        assert!(&vec3 != &vec_red);
       
    }
    #[test]
    fn example1_test() {
       assert_eq!(example1(), 8);
       
    }
}
}