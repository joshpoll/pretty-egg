use egg::{rewrite as rw, *};

/* Polynomial-Time Optimal Pretty-Printing Combinators with Choice */
/* Prettiest */

define_language! {
  enum Doc {
    // TODO: make the operators n-ary?
    "<>" = HCat([Id; 2]),
    "flush" = Flush(Id),
    "<|>" = Choice([Id; 2]),
    "!!" = Decision([Id; 2]),
    // TODO: these operators (except text) are derived, and don't really need to be in the e-graph!
    "$$" = VCat([Id; 2]),
    Text(Symbol),
    Uid(u32),
  }
}

/* TODO: need to get extraction rules from somewhere. They are in the paper, but in kind of a weird way. */
/* TODO: probably need to keep track of the parameters of the text boxes in the cost function computation, but then write a custom ordering on the cost type */
/* TODO: maybe adopt parameter names from prettiest: height, lastWidth, maxWidth */
/* TODO: what about using flush like prettiest? */

const WIDTH: usize = 80;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
struct BoxDims {
  height: usize,
  width: usize,
  last_width: usize,
  // TODO: this is a hack so I can get the pretty-printed output from the cost.
  // it is probably possible to compute the layout just for the extracted term
  layout: Vec<String>,
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
      Doc::HCat([a, b]) => {
        let a_cost = costs(*a);
        let b_cost = costs(*b);
        match (a_cost, b_cost) {
          (LayoutCost::Infinity, _) => LayoutCost::Infinity,
          (_, LayoutCost::Infinity) => LayoutCost::Infinity,
          (LayoutCost::BoxDims(a_bd), LayoutCost::BoxDims(b_bd)) => {
            let mut layout: Vec<String> = vec![];
            if let (Some((a_lst, a_init)), Some((b_fst, b_tail))) = (a_bd.layout.split_last(), b_bd.layout.split_first()) {
              layout.append(&mut a_init.clone().to_vec());
              layout.push(a_lst.clone() + b_fst);
              let indent = " ".repeat(a_lst.len());
              layout.append(&mut b_tail.iter().map(|b| indent.clone() + b).collect());
            }
            LayoutCost::BoxDims(BoxDims {
              width: std::cmp::max(a_bd.width, a_bd.last_width + b_bd.width),
              height: a_bd.height + b_bd.height,
              last_width: a_bd.last_width + b_bd.last_width,
              layout,
            })
        },
        }
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
      // we don't want these nodes in our final layout!
      Doc::VCat(_) => LayoutCost::Infinity,
      Doc::Choice(_) => LayoutCost::Infinity,
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

fn main() {
  let rules: &[Rewrite<Doc, ()>] = &[
    rw!("vcat"; "($$ ?top ?bot)" => "(<> (flush ?top) ?bot)"),
    rw!("choice-left"; "(<|> ?left ?right)" => "?left"),
    rw!("choice-right"; "(<|> ?left ?right)" => "?right"),
  ];

  // let start = "foo".parse().unwrap();
  // let start = "(flush foo)".parse().unwrap();
  // let start = "(<|> foo bar)".parse().unwrap();
  let start = "(<> [ (<> (<|> ($$ a b) (<> a (<> _ b))) ]))".parse().unwrap();
  // let start = "(<> [ (<> (<|> (<|> a (<|> b (<|> c d))) (<> a (<> _ (<> b (<> _ (<> c (<> _ d))))))) ]))".parse().unwrap();

  let runner = Runner::default().with_expr(&start).run(rules);

  let mut extractor = Extractor::new(&runner.egraph, PrinterCostFn);

  let (best_cost, best_expr) = extractor.find_best(runner.roots[0]);

  println!("best expr: {}", best_expr);
  println!("best expr: {:?}", best_expr);
  println!("best cost: {:?}", best_cost);
  println!("pretty print:\n\n{}", pretty_print(&best_cost));
}
