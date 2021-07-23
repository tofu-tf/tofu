package tofu.doobie.example

final case class Person(id: Long, name: String, deptId: Long)
final case class Dept(id: Long, name: String)