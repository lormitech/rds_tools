
//
// version:       0.1.0
// last modified: 31.10.12
//


//
// "rds_json.js" - A Javascript library to process Respondent Driven
//                 Sampling (RDS) data
//
// in particular ...
//   a) to generate an RDS tree in JSON format
//   b) to convert an RDS tree in JSON format to a tree in DOT format
//

//--


//
// -- RDS tree ---
//
// note that ...
//   - each node corresponds to a participant;
//   - each partecipant can be identified by one of these values:
//       [id1] = the respondent ID; value in the first column of the RDS data set;
//       [id2] = the position, i.e. row number, in the original RDS data set
//   - a participant can be either a seed or a respondent;
//     for a seed, the respondent ID (id1) is equal to -1;
//

//
// a JSON tree structure is a hash (i.e. {});
// it consists of nodes, each having as properties:
//
//   id      [string] = unique identifier for the node
//   name    [string] = name of the node
//   data    [object] = optional property containing a hash (i.e. {}) where
//                      it is possible to store info/data about the node
//   children [array] = optional property containing an array (i.e. []) where
//                      it is possible to store the children of the node
//
//
  

// constructor for a tree in JSON format
function tree_json(id, name, data, children) {

  // `id' coincides with `id2'
  this.id = id;
  // `name' coincides with `id1'
  this.name = name;
  this.data = data;
  this.children = children;
  this.addChild = function(new_child) {
    this.children.push(new_child);
  }  

}


// function to perform the concatenation test
// (recursive function)
function concatenationTest(stree, stree_ref) {
	
  var concat_out = new Array();
  // initiliaze the data structure `concat_out':
  // if the concatenation test fails, the function
  // returns the original subtree
  concat_out.push(stree);
  concat_out.push(0);		  
  var concat_out_c;
 
  // if the subtree has children, perform the
  // concatenation test on them ...
  if (stree.children != 0) {
    for (var k=0; k<stree.children.length; k++) {	
      concat_out_c = concatenationTest(stree.children[k], stree_ref);
      if (concat_out_c[1] == 1) {
        stree.children[k] = concat_out_c[0];
        concat_out[0] = stree; 
        concat_out[1] = concat_out_c[1];
        return(concat_out);
      }
    }
  }
 
  // concatenation test
  if (stree.id == stree_ref.id) {
    for (var k=0; k<stree_ref.children.length; k++) {
      stree.addChild(stree_ref.children[k]);
    }	
    concat_out[0] = stree;
    concat_out[1] = 1;	   
    return(concat_out);
  }
	 
  return(concat_out);

}


// function to concatenate the subtrees
function concatenateJSONSubTrees(strees_json) {

  // note that, in order to optimize the algorithm, the array
  // `strees_json' is shrinked and as a result the number of subtrees
  // to process is reduced at each concatenation step

  var strees_json_t = strees_json.slice();
  var concat_out;
  var sc = 0;
	
  for (var i=0; i<strees_json_t.length; i++) {
    for (var j=0; j<strees_json.length; j++) {
      for (var k=0; k<strees_json[j].children.length; k++) {
        // concatenation test
	concat_out = concatenationTest(strees_json[j].children[k], strees_json_t[i]);
        if (concat_out[1] == 1) { 		
          strees_json[j].children[k] = concat_out[0];
          // shrinking the array
          strees_json.splice((i - sc), 1);
          sc = sc + 1;  
          break;
        }
      }
      if (concat_out[1] == 1) break;
    }
  }
  
  return(strees_json);
  
}


// function to get the parents for each node pairs
function getParents(node_pairs) {

  var parents = Array();
  for (var i=0; i<node_pairs.length; i++) {
    if (i%2==0) parents.push(node_pairs[i]);  
  }
  
  return(parents);

}


// function to generate the subtrees
function generateJSONSubTrees(node_pairs, rds_data) {

  var idx_p, idx_c, no_strees_c, idx_t, start;
  var strees_json = new Array();
  
  var pa = getParents(node_pairs);
  pa = pa.sort(function(a,b){return a-b});
  pa_u = pa.unique();
   
  for (var ip=0; ip<pa_u.length; ip++) {

    var idxs_c = new Array();
    idx_p = 0;
    idx_c = 0;
    no_strees_c = 0;
    idx_t = 0;
    start = 0;
    while (idx_t != -1) {
      idx_t = node_pairs.indexOf(pa_u[ip], start);
      if ((idx_t != -1) && (idx_t%2 == 0)) {
        idx_p = idx_t;
        start = idx_t + 1;
        idx_c = idx_p + 1;
        idxs_c.push(idx_c);
        no_strees_c = no_strees_c + 1;
      }
      if (idx_t%2 != 0) start = idx_t + 1;
    }
 	   	
    // parent
    var stree_p = {};
    tree_json.call(stree_p, node_pairs[idx_p], rds_data[node_pairs[idx_p]][0], {}, []);
    // children
    for (var ic=0; ic<no_strees_c; ic++) {
      var stree_c = {};
      tree_json.call(stree_c, node_pairs[idxs_c[ic]], rds_data[node_pairs[idxs_c[ic]]][0], {}, []);
      stree_p.addChild(stree_c);
    }
    strees_json.push(stree_p);
	
  }

  return(strees_json);
  
}


// function to generate the tree
function generateJSONTree(node_pairs, rds_data) {
  
  var strees_json = generateJSONSubTrees(node_pairs, rds_data);  
  var tree_json = concatenateJSONSubTrees(strees_json);
     
  return(tree_json);
  
}


// function to get the node pairs of the tree;
// note that this is a `recursive' function (tree traversal)
function getNodePairsFromJSON(tree_json, node_pairs) {

  var parent_name = tree_json.name;
  var children = tree_json.children;

  if (children.length==0) return(node_pairs);

  for (var i=0; i<children.length; i++) {
    // storing current node pair
    node_pairs.push(parent_name);
    node_pairs.push(children[i].name);
    // traversing the tree, processing the next subtree
    getNodePairsFromJSON(children[i], node_pairs);
  }
  
  return(node_pairs);
  
}


// fuction to convert a tree in JSON format
// to a tree in DOT format
function fromJSONtoDOT(tree_json) {

  var node_pairs = new Array();
  node_pairs = getNodePairsFromJSON(tree_json, node_pairs);
 
  var tree_dot = "digraph rds_graph { ";
  for (var i=0; i<node_pairs.length; i++) {
    tree_dot = tree_dot + node_pairs[i] + " -> " + node_pairs[(i+1)] + "; ";
	i = i + 1;
  }
  tree_dot = tree_dot + " }";
  
  return(tree_dot);
    
}
