class Test {
 public function new() {} 
 static function f() {
        g();
 }
 static function g() {
        f();
 }
 static function main() {
	var x  = 5;
	var y = x+x;
 }
 private function cycle() {
	return new Test2();
 }
}

class Test2 {
  public function new() {}
  public function cycle2() {
	return new Test();
  }
}
