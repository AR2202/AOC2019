pub mod day3a{
    use std::iter::FromIterator;
    extern crate array_tool;
    use array_tool::vec::*;
    use std::fs;
    use std::str::FromStr;


    #[derive(Debug,PartialEq,Clone)]
    struct Coordinate (i32,i32);
    #[derive(Debug)]
    enum Direction {
        R,
        L,
        U,
        D
    }
    #[derive(Debug)]
    struct Segment (Direction,Distance);
    
    type Distance = i32;
    
    type Wire =Vec<Coordinate>;
    #[derive(Debug)]
    struct ParseDirectionError { /* fields omitted */ }
    
    impl FromStr for Direction {
        type Err = ParseDirectionError;
    
        
        #[inline]
        fn from_str(s: &str) -> Result<Direction, ParseDirectionError> {
            match s {
                "D"  => Ok(Direction::D),
                "R" => Ok(Direction::R),
                "L" => Ok(Direction::L),
                "U" => Ok(Direction::U),
                _   => Err(ParseDirectionError {/*fields*/}),
            }
        }
    }








    
    
    fn make_wire(coord: Coordinate, seg: &Segment ) -> Wire
    {   
        match seg {
           Segment (Direction::R ,a)=> push_to_x(coord.0,coord.0+a,coord.1),
    
           Segment (Direction::L,a)=>rev_push_to_x(coord.0-a,coord.0,coord.1),
            
           Segment (Direction::D,a)=>rev_push_to_y(coord.1-a,coord.1,coord.0),
           Segment (Direction::U,a)=>push_to_y(coord.1,coord.1+a,coord.0)
        
        
        }
    }
    fn extend_wire(seg: &Segment, mut wire: Wire) -> Wire
    {
        
        let maybelast: Option<Coordinate> = wire.pop();
        let last:Coordinate = check_maybe_coord(maybelast);
        let newWire : Wire = make_wire(last, seg);
        
        let resultvec: Wire = wire.into_iter().chain(newWire).collect();
        return resultvec


    }
    fn check_maybe_coord(opt:Option<Coordinate>)->Coordinate
    {
        match opt {
            None => Coordinate(0,0),
            Some(c)=>c
        }
    }
    
    fn push_to_x(startval:i32, endval:i32,yval:i32)->Vec<Coordinate>
    {
        let mut vector :Vec<Coordinate>= Vec::new();
        let mut index = startval;
        while index<(endval+1) {
            vector.push(Coordinate(index,yval));
            index+=1;
        }
        return vector
    }
    fn push_to_y(startval:i32, endval:i32,xval:i32)->Vec<Coordinate>
    {
        let mut vector :Vec<Coordinate>= Vec::new();
        let mut index = startval;
        while index<(endval+1) {
            vector.push(Coordinate(xval,index));
            index+=1;
        }
        return vector
    }
    fn rev_push_to_x(startval:i32,endval:i32,yval:i32)->Vec<Coordinate>
    {
        let mut vector :Vec<Coordinate> = push_to_x(startval, endval, yval);
        let oldwire = vector.into_iter();
        let reversed :Vec<Coordinate>= oldwire.rev().collect();
        return reversed

    }

    fn rev_push_to_y(startval:i32,endval:i32,xval:i32)->Vec<Coordinate>
    {
        let mut vector :Vec<Coordinate> = push_to_y(startval, endval, xval);
        let oldwire = vector.into_iter();
        let reversed :Vec<Coordinate>= oldwire.rev().collect();
        return reversed

    }
    fn whole_wire(seglist:&Vec<Segment>)->Wire{
        let mut v: Wire= vec![Coordinate (0,0)];
        let wire = seglist.iter().fold(v, |acc, x| extend_wire(x,acc));
        return wire

    }
    fn manhattendistance(coord:&Coordinate)->i32{
        coord.0.abs() +coord.1.abs()
    }
    fn car_cdr(s: &str) -> (&str, &str) {
        match s.chars().next() {
            Some(c) => s.split_at(c.len_utf8()),
            None => s.split_at(0),
        }
    }
    pub fn day3a_sol(){
        
        let filename = "../../input/day3.txt";
        let contents = fs::read_to_string(filename);
    
    
    
        let contents2= contents.unwrap();
        let thelines = contents2.lines();
        

        let mut seglists:Vec<Vec<Segment>>=Vec::new();
        for s in thelines {
            let mut segmentlist :Vec<Segment>= Vec::new();
            
            let segs: Vec<&str> = s.split(',').collect();
            for seg in segs {
                let (first_char, remainder) = car_cdr(seg);
                let dir:Direction = Direction::from_str(first_char).unwrap();
                let dist:Distance = i32::from_str(remainder).unwrap();
                let segment = Segment(dir,dist);
                segmentlist.push(segment);
                
            }
            seglists.push(segmentlist);
            


        }
        
        let wholewire1=whole_wire(&seglists[0]);
        let wholewire2=whole_wire(&seglists[1]);
        let intersections=wholewire1.intersect(wholewire2);
        
        let manhattendistances:Vec<i32>= intersections.iter()
                                                      .filter(|y| **y!= Coordinate(0,0))
                                                      .map( |x| manhattendistance(x)).collect();
        let mindist = manhattendistances.iter().min();
        match mindist {
            Some(min) => println!( "Min value: {}", min ),
            None      => println!( "Vector is empty" ),
        }

    }
}