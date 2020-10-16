pub mod day6{
    use petgraph::graph::Graph;
    use petgraph::graph::node_index;
    use petgraph::graph::NodeIndex;
    use petgraph::dot::Dot;
    use petgraph_evcxr::draw_graph;
    use petgraph::algo::astar;
    use petgraph::algo::dijkstra;
    use std::fs;
    use std::collections::HashMap;
    use array_tool::vec::Uniq;
    

    fn makeGraph() ->Graph <(),()>{
        let mut graph = Graph::<(), ()>::new(); // directed and unlabeled
    
        graph.extend_with_edges(&[ (0, 1) ]);
        return graph
    
        
    }
    pub fn makeExampleGraph1() ->Graph <char,i32>{
        let mut graph = Graph::<char, i32>::new(); // directed and unlabeled
        let b = graph.add_node('B');
        let o = graph.add_node('O');
        graph.add_edge(o, b,1 );
        let g = graph.add_node('G');
        graph.add_edge(b,g,1);
        let h = graph.add_node('H');
        graph.add_edge(g,h,1);
        let c = graph.add_node('C');
        graph.add_edge(b,c,1);
        let d = graph.add_node('D');
        graph.add_edge(c,d,1);
        let i = graph.add_node('I');
        graph.add_edge(d,i,1);
        let e = graph.add_node('E');
        graph.add_edge(d,e,1);
        let f = graph.add_node('F');
        graph.add_edge(e,f,1);
        let j = graph.add_node('J');
        graph.add_edge(e,j,1);
        let k = graph.add_node('K');
        graph.add_edge(j,k,1);
        let l = graph.add_node('L');
        graph.add_edge(k,l,1);
        
        
        
        let graph_of_orbits = dijkstra(&graph, o, None, |_| 1);
        let iterator = graph_of_orbits.into_iter();
        let ntotalorbits:i32 = iterator.map(|(_,v)| v).sum();
        println!("The total number of orbits is: {:?}",&ntotalorbits);
        return graph
    
        
    }

    
    pub fn day6a_sol(){
        
        let filename = "../input/day6.txt";
        let contents = fs::read_to_string(filename);
    
    
    
        let contents2= contents.unwrap();
        let thelines = contents2.lines();
        

        let mut tuplelist :Vec<(&str,&str)>=Vec::new();
        for s in thelines {
            let obj:Vec<&str> = s.split(")").collect();
            let tup:(&str,&str)=(obj[0],obj[1]);
            tuplelist.push(tup);
            
        


        }
        let firsts : Vec<&str>= tuplelist.iter().map(|tuple| tuple.0).collect();
        let seconds : Vec<&str>=tuplelist.iter().map(|tuple| tuple.1).collect();
        let firstonly : Vec<&&str> = firsts.iter().filter(|i|!seconds.contains(&i)).collect();
        println!("{:?}",&firstonly); 
        let mut map_of_celetial_bodies = HashMap::new();
        map_of_celetial_bodies.insert(
            firstonly[0].to_string(),
            0,
        );
        for s in seconds.unique().iter(){
            if !map_of_celetial_bodies.contains_key(&s.to_string()){
            map_of_celetial_bodies.insert(
                s.to_string(),
                map_of_celetial_bodies.len() as u32,
            );
        }
        
        }
        let edgelist : Vec<(u32,u32)>= tuplelist.iter().map(|tuple| (*map_of_celetial_bodies.get(tuple.0).unwrap(),*map_of_celetial_bodies.get(tuple.1).unwrap())).collect();
        println!("We've got {} keys.",
        map_of_celetial_bodies.len());
        
        let mut graph = Graph::<(), i32>::from_edges(&edgelist); 
        let graph_of_orbits = dijkstra(&graph, node_index(0), None, |_| 1);
        let iterator = graph_of_orbits.into_iter();
        let ntotalorbits:i32 = iterator.map(|(_,v)| v).sum();
        println!("The total number of orbits is: {:?}",&ntotalorbits);
        
        
        
    }
#[cfg(test)]
mod tests {
    use super::*;
    
    
    #[test]

    fn graph_test() {
        let graph = makeGraph();
        assert_eq!(graph.node_count(), 2);
        assert_eq!(graph.edge_count(), 1);
        
       
    }
    #[test]
    fn graphExample1_test() {
        let graph = makeExampleGraph1();
        assert_eq!(graph.node_count(), 12);
        assert_eq!(graph.edge_count(), 11);
        
        
       
    }
    
}
}