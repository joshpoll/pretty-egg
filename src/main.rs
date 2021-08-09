use egg::{rewrite as rw, *};
use reduce::Reduce;

/* Polynomial-Time Optimal Pretty-Printing Combinators with Choice */
/* Prettiest */

define_language! {
  enum Doc {
    // TODO: make the operators n-ary?
    "<>" = HCat(Vec<Id>),
    "flush" = Flush(Id),
    "<|>" = Choice([Id; 3]),
    "!!" = Decision([Id; 2]),
    // TODO: implement this!!!
    // "_" = Space(Id),
    Text(Symbol),
    Uid(usize),
  }
}

/* TODO: need to get extraction rules from somewhere. They are in the paper, but in kind of a weird way. */
/* TODO: probably need to keep track of the parameters of the text boxes in the cost function computation, but then write a custom ordering on the cost type */
/* TODO: maybe adopt parameter names from prettiest: height, lastWidth, maxWidth */
/* TODO: what about using flush like prettiest? */

const WIDTH: usize = 10;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
struct BoxDims {
  height: usize,
  width: usize,
  last_width: usize,
  // TODO: this is a hack so I can get the pretty-printed output from the cost.
  // it is probably possible to compute the layout just for the extracted term
  layout: Vec<String>,
}

impl Default for BoxDims {
  fn default() -> Self {
    BoxDims {
      height: 0,
      width: 0,
      last_width: 0,
      layout: vec![],
    }
  }
}

// TODO: Infinity basically acts like an Option, so maybe replace it with that?
// or otherwise make it more monadic/ergonomic to deal with
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
enum LayoutCost {
  BoxDims(BoxDims),
  Infinity,
}

struct PrinterCostFn;
impl CostFunction<Doc> for PrinterCostFn {
  type Cost = LayoutCost;
  fn cost<C>(&mut self, enode: &Doc, mut costs: C) -> Self::Cost
  where
    C: FnMut(Id) -> Self::Cost,
  {
    let free_cost = match enode {
      Doc::Text(s) => LayoutCost::BoxDims(BoxDims {
        height: 0,
        width: s.as_str().len(),
        last_width: s.as_str().len(),
        layout: vec![s.as_str().to_string()],
      }),
      Doc::HCat(ds) => {
        let ds_costs = ds.into_iter().map(|&d| costs(d));
        ds_costs.into_iter().reduce(|acc_cost, d_cost| {
          match (acc_cost, d_cost) {
            (LayoutCost::Infinity, _) => LayoutCost::Infinity,
            (_, LayoutCost::Infinity) => LayoutCost::Infinity,
            (LayoutCost::BoxDims(acc_bd), LayoutCost::BoxDims(d_bd)) => {
              let mut layout: Vec<String> = vec![];
              if let (Some((a_lst, a_init)), Some((b_fst, b_tail))) = (acc_bd.layout.split_last(), d_bd.layout.split_first()) {
                layout.append(&mut a_init.clone().to_vec());
                layout.push(a_lst.clone() + b_fst);
                let indent = " ".repeat(a_lst.len());
                layout.append(&mut b_tail.iter().map(|b| indent.clone() + b).collect());
              };
              LayoutCost::BoxDims(BoxDims {
                width: std::cmp::max(acc_bd.width, acc_bd.last_width + d_bd.width),
                height: acc_bd.height + d_bd.height,
                last_width: acc_bd.last_width + d_bd.last_width,
                layout,
              })
          },
          }
        }).unwrap()
      }
      Doc::Flush(a) => {
        let a_cost = costs(*a);
        match a_cost {
          LayoutCost::Infinity => LayoutCost::Infinity,
          LayoutCost::BoxDims(a_bd) => {
            let mut layout = a_bd.clone().layout;
            layout.push("".to_string());
            LayoutCost::BoxDims(BoxDims {
              width: a_bd.width,
              height: a_bd.height + 1,
              last_width: 0,
              layout,
            })
          },
        }
      }
      // we don't want choice in our final layout!
      Doc::Choice(_) => LayoutCost::Infinity,
      Doc::Uid(_) => LayoutCost::BoxDims(BoxDims {
        width: 0,
        height: 0,
        last_width: 0,
        layout: vec![],
      }),
      Doc::Decision([_, a]) => costs(*a),
    };

    // ignore layouts that extend beyond WIDTH
    match free_cost {
      LayoutCost::Infinity => LayoutCost::Infinity,
      LayoutCost::BoxDims(BoxDims { width, .. }) => {
        if width > WIDTH {
          LayoutCost::Infinity
        } else {
          free_cost
        }
      }
    }
  }
}

fn pretty_print(cost: &LayoutCost) -> String {
  match cost {
    LayoutCost::Infinity => "Error, Infinite cost!".to_string(),
    LayoutCost::BoxDims(BoxDims { layout, .. }) => layout.join("\n")
  }
}

#[derive(Debug, Clone)]
enum DocAST {
  // TODO: make the operators n-ary?
  HCat(Vec<DocAST>),
  VCat(Vec<DocAST>),
  Flush(Box<DocAST>),
  Choice(Box<DocAST>, Box<DocAST>),
  Text(String),
  // TODO: implement this!!!
  // Space(u32),
}

// https://users.rust-lang.org/t/idiomatic-rust-way-to-generate-unique-id/33805/2
use std::sync::atomic::{AtomicUsize, Ordering};
fn get_id() -> usize {
    static COUNTER:AtomicUsize = AtomicUsize::new(1);
    COUNTER.fetch_add(1, Ordering::Relaxed)
}

fn lower_doc_ast(doc: &DocAST) -> DocAST {
  match doc {
    DocAST::HCat(ds) => DocAST::HCat(ds.into_iter().map(lower_doc_ast).collect()),
    DocAST::VCat(ds) =>
      ds.into_iter()
        .map(lower_doc_ast)
        .reduce(|acc, d| DocAST::HCat(vec![DocAST::Flush(Box::new(acc)), d]))
        .unwrap(),
    DocAST::Flush(d) => DocAST::Flush(Box::new(lower_doc_ast(d))),
    DocAST::Choice(l, r) => DocAST::Choice(Box::new(lower_doc_ast(l)), Box::new(lower_doc_ast(r))),
    DocAST::Text(s) => DocAST::Text(s.clone()),
  }
}

fn construct_rec_expr(doc: &DocAST, expr: &mut RecExpr<Doc>) -> Result<Id, String> {
  match doc {
    DocAST::HCat(ds) => {
      let parsed_ds = ds.into_iter().map(|d| construct_rec_expr(d, expr)).collect::<Result<Vec<_>, _>>()?;
      let node = Doc::from_op_str("<>", parsed_ds)?;
      Ok(expr.add(node))
    },
    DocAST::VCat(_) => Err("VCat not allowed! Did you call lower_doc_ast first?".to_string()),
    DocAST::Flush(d) => {
      let parsed_d = construct_rec_expr(d, expr)?;
      let node = Doc::from_op_str("flush", vec![parsed_d])?;
      Ok(expr.add(node))
    },
    DocAST::Choice(l, r) => {
      let parsed_l = construct_rec_expr(l, expr)?;
      let parsed_r = construct_rec_expr(r, expr)?;
      let uid = expr.add(Doc::from_op_str(get_id().to_string().as_str(), vec![])?);
      let node = Doc::from_op_str("<|>", vec![uid, parsed_l, parsed_r])?;
      Ok(expr.add(node))
    },
    DocAST::Text(s) => {
      let node = Doc::from_op_str(s, vec![])?;
      Ok(expr.add(node))
    }
    // DocAST::Space(_) => Err("foo"),
  }
}

fn sexp_doc(sexp: &Vec<String>) -> DocAST {
  // TODO: this is gross
  // TODO: get open and closing braces to share lines with first and last entries
  let mut tokens = vec![DocAST::Text("[".to_string())];
  let mut space_tokens = vec![DocAST::Text("[".to_string())];

  let mut sexp_iter = sexp.into_iter();

  if let Some(e) = sexp_iter.next() {
    tokens.push(DocAST::Text(e.to_string()));
    space_tokens.push(DocAST::Text(e.to_string()));

    for e in sexp_iter {
      space_tokens.push(DocAST::Text(" ".to_string()));

      tokens.push(DocAST::Text(e.to_string()));
      space_tokens.push(DocAST::Text(e.to_string()));
    }
  }

  tokens.push(DocAST::Text("]".to_string()));
  space_tokens.push(DocAST::Text("]".to_string()));

  DocAST::Choice(
    Box::new(DocAST::HCat(space_tokens)),
    Box::new(DocAST::VCat(tokens)),
  )
}

fn main() {
  let rules: &[Rewrite<Doc, ()>] = &[
    rw!("choice-left"; "(<|> ?uid ?left ?right)" => "(!! ?uid ?left)"),
    rw!("choice-right"; "(<|> ?uid ?left ?right)" => "(!! ?uid ?right)"),
  ];

  // let start = "foo".parse().unwrap();
  // let start = "(flush foo)".parse().unwrap();
  // let start = "(<|> foo bar)".parse().unwrap();
  // let start_ast =
  //   DocAST::VCat(
  //     vec![DocAST::Text("[".to_string()), DocAST::Text("a".to_string()), DocAST::Text("b".to_string()), DocAST::Text("]".to_string())]
  //   );
  let start_ast = sexp_doc(&vec!["a", "b", "c", "d", "e"].into_iter().map(String::from).collect());

  let mut start = RecExpr::default();
  construct_rec_expr(&lower_doc_ast(&start_ast), &mut start).unwrap();

  // let start = "(<> [ (<> (<|> 0 ($$ a b) (<> a (<> _ b))) ]))".parse().unwrap();
  // let start = "(<> [ (<> (<|> (<|> a (<|> b (<|> c d))) (<> a (<> _ (<> b (<> _ (<> c (<> _ d))))))) ]))".parse().unwrap();

  let runner = Runner::default().with_expr(&start).run(rules);

  let mut extractor = Extractor::new(&runner.egraph, PrinterCostFn);

  let (best_cost, best_expr) = extractor.find_best(runner.roots[0]);

  println!("best expr: {}", best_expr);
  println!("best expr: {:?}", best_expr);
  println!("best cost: {:?}", best_cost);
  println!("pretty print:\n-------------\n{}", pretty_print(&best_cost));
}
