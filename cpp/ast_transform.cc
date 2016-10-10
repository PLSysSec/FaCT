#include <iostream>
#include <vector>
#include <set>
#include <cassert>

using namespace std;

enum class StType {
  Init,
  Assign,
  IfElse,
  Return,
};

class Statement {
public:
  StType sttype;

  Statement(StType sttype) : sttype(sttype) { }
  virtual ostream& s (ostream& o) const { return o; }
};
ostream& operator<<(ostream& o, const Statement& x);

enum class ExprType {
  UnExpr,
  BinExpr,
  Const,
  Var,
};

class Expr {
public:
  ExprType etype;

  Expr(ExprType etype) : etype(etype) { }
  virtual ostream& s (ostream& o) const { return o; }
};
ostream& operator<<(ostream& o, const Expr& x);

class Init : public Statement {
public:
  string name;
  Expr* val;

  Init(string name, Expr* val) : Statement(StType::Init), name(name), val(val) { }
  virtual ostream& s (ostream& o) const {
    return o << name << " := " << *val;
  }
};

class Const : public Expr {
public:
  int c;

  Const(int c) : Expr(ExprType::Const), c(c) { }
  virtual ostream& s (ostream& o) const {
    return o << c;
  }
};

class Var : public Expr {
public:
  string name;

  Var(string name) : Expr(ExprType::Var), name(name) { }
  virtual ostream& s (ostream& o) const {
    return o << name;
  }
};

enum class Op {
  Add,
  Sub,
  BitAnd,
  BitOr,
  BitNot,
  LessThan,
};

class UnExpr : public Expr {
public:
  Op op;
  Expr* e;

  UnExpr(Op op, Expr* e) : Expr(ExprType::BinExpr), op(op), e(e) { }
  virtual ostream& s (ostream& o) const {
    string opstr = "";
    switch (op) {
      case Op::BitNot:
        opstr = "~"; break;
    }
    return o << "(" << opstr << *e << ")";
  }
};

class BinExpr : public Expr {
public:
  Op op;
  Expr* e1;
  Expr* e2;

  BinExpr(Op op, Expr* e1, Expr* e2) : Expr(ExprType::BinExpr), op(op), e1(e1), e2(e2) { }
  virtual ostream& s (ostream& o) const {
    string opstr = "";
    switch (op) {
      case Op::Add:
        opstr = " + "; break;
      case Op::Sub:
        opstr = " - "; break;
      case Op::BitAnd:
        opstr = " & "; break;
      case Op::BitOr:
        opstr = " | "; break;
      case Op::LessThan:
        opstr = " < "; break;
    }
    return o << "(" << *e1 << opstr << *e2 << ")";
  }
};


class Assign : public Statement {
public:
  string var;
  Expr* e;

  Assign(string var, Expr* e) : Statement(StType::Assign), var(var), e(e) { }
  virtual ostream& s (ostream& o) const {
    return o << var << " = " << *e;
  }
};

class IfElse : public Statement {
public:
  Expr* b;
  vector<Statement*> t;
  vector<Statement*> f;

  IfElse(Expr* b, vector<Statement*> t, vector<Statement*> f) : Statement(StType::IfElse), b(b), t(t), f(f) { }
  virtual ostream& s (ostream& o) const {
    o << "if " << *b << endl << "then" << endl;
    for (auto s : t) {
      o << *s << endl;
    }
    o << "else" << endl;
    for (auto s : f) {
      o << *s << endl;
    }
    o << "end";
    return o;
  }
};

class Return : public Statement {
public:
  Expr* r;

  Return(Expr* r) : Statement(StType::Return), r(r) { }
  virtual ostream& s (ostream& o) const {
    return o << "return " << *r;
  }
};

class Function {
public:
  string name;
  vector<string> params;
  vector<Statement*> body;

  Function(string name, vector<string> params, vector<Statement*> body) : name(name), params(params), body(body) { }
  virtual ostream& s (ostream& o) const {
    o << name << "(";
    for (auto x : params) {
      o << x << ",";
    }
    o << "):" << endl;
    for (auto s : body) {
      o << *s << endl;
    }
    return o;
  }
};
ostream& operator<<(ostream& o, const Function& x);

ostream& operator<<(ostream& o, const Statement& x) { return x.s(o); }
ostream& operator<<(ostream& o, const Expr& x) { return x.s(o); }
ostream& operator<<(ostream& o, const Function& x) { return x.s(o); }

Expr* rename(string x, string y, Expr* e) {
  switch (e->etype) {
    case ExprType::UnExpr:
      {
        UnExpr* u = (UnExpr*)e;
        return new UnExpr(u->op, rename(x, y, u->e));
      }
      break;
    case ExprType::BinExpr:
      {
        BinExpr* b = (BinExpr*)e;
        return new BinExpr(b->op, rename(x, y, b->e1), rename(x, y, b->e2));
      }
      break;
    case ExprType::Var:
      {
        Var* v = (Var*)v;
        if (v->name == x) {
          return new Var(y);
        }
      }
      break;
  }
  return e;
}

Statement* rename(string x, string y, Statement* s) {
  switch (s->sttype) {
    case StType::Init:
      {
        Init* init = (Init*)s;
        if (init->name == x)
          return new Init(y, rename(x, y, init->val));
      }
      break;
    case StType::Assign:
      {
        Assign* assign = (Assign*)s;
        if (assign->var == x)
          return new Assign(y, rename(x, y, assign->e));
      }
      break;
    default:
      assert(false);
  }
  return s;
}

vector<Statement*> rename(string x, string y, vector<Statement*> ss) {
  vector<Statement*> v;
  for (auto s : ss) {
    v.push_back(rename(x, y, s));
  }
  return v;
}

vector<Statement*> transform(vector<Statement*> ss, set<string>& assigns);
vector<Statement*> transformite(IfElse *ite) {
  static int ctr = 1;
  auto b = new Init(string("b") + to_string(ctr++), ite->b);
  set<string> assigns;
  auto tbody = transform(ite->t, assigns);
  auto fbody = transform(ite->f, assigns);
  vector<Statement*> finals;
  for (auto x : assigns) {
    auto xt = x + to_string(ctr++);
    auto xf = x + to_string(ctr++);
    tbody = rename(x, xt, tbody);
    tbody.insert(tbody.begin(), new Init(xt, new Const(0)));
    fbody = rename(x, xf, fbody);
    fbody.insert(fbody.begin(), new Init(xf, new Const(0)));
    finals.push_back(new Assign(x, new BinExpr(Op::BitOr,
            new BinExpr(Op::BitAnd, new Var(b->name), new Var(xt)),
            new BinExpr(Op::BitAnd, new UnExpr(Op::BitNot, new Var(b->name)), new Var(xf)))));
  }
  vector<Statement*> v;
  v.push_back(b);
  v.insert(v.end(), tbody.begin(), tbody.end());
  v.insert(v.end(), fbody.begin(), fbody.end());
  v.insert(v.end(), finals.begin(), finals.end());
  return v;
}

vector<Statement*> transform(Statement* s, set<string>& assigns) {
  switch (s->sttype) {
    case StType::IfElse:
      return transformite((IfElse*)s);
    case StType::Assign:
      assigns.insert(((Assign*)s)->var);
    default:
      return {s};
  }
}

vector<Statement*> transform(vector<Statement*> ss, set<string>& assigns) {
  vector<Statement*> v;
  for (auto s : ss) {
    auto n = transform(s, assigns);
    v.insert(v.end(), n.begin(), n.end());
  }
  return v;
}

vector<Statement*> transform(vector<Statement*> ss) {
  set<string> v;
  return transform(ss, v);
}

Function transform(Function f) {
  auto body = transform(f.body);
  return Function(f.name, f.params, body);
}

int main() {
  Function f ("simple", {"x"}, {
      new Init("y", new Const(2)),
      new Init("z", new Const(4)),
      new IfElse(
          new BinExpr(Op::LessThan, new Var("x"), new Const(5)),
          {new Assign("y", new BinExpr(Op::Add, new Var("y"), new Const(3)))},
          {new Assign("z", new BinExpr(Op::Sub, new Var("z"), new Const(7)))}),
      new Return(new BinExpr(Op::Add, new Var("y"), new Var("z")))});

  cout << "Original function:" << endl;
  cout << f << endl;

  Function newf = transform(f);

  cout << "Transformed function:" << endl;
  cout << newf << endl;

  return 0;
}
