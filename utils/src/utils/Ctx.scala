package utils

trait Ctx {
  def file: String
  def line: Int
  def col: Int
  def content: String
  def caret: String = " "*(col-1) + "^"
}
