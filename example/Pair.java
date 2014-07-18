
class Pair extends Object {
  Object lhs;
  Object rhs;

  Pair(Object lhs, Object rhs) {
    super();
    this.lhs = lhs;
    this.rhs = rhs;
  }

  Pair cons(Object obj) {
    return new Pair(obj, this);
  }

  Pair cons(Pair pair) {
    return new Pair(pair.lhs, new Pair(pair.rhs, this));
  }
}

