package patmat

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import patmat.Huffman._

@RunWith(classOf[JUnitRunner])
class HuffmanSuite extends FunSuite {
  trait TestTrees {
    val t0 = Leaf('a', 2)
    val t1 = Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5)
    val t2 = Fork(Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5),
      Leaf('d',4), List('a','b','d'), 9)
    val str = "xtxextx"
    val leaflist = string2Chars(str)
    val timesleaflist = times(leaflist)
    val makeorderedleaflist = makeOrderedLeafList(timesleaflist)
    val combinelist = combine(makeorderedleaflist)
    val finallist = combine(combinelist)
    val finalCodeTree = createCodeTree(string2Chars(str))
    val codeStr = "001010011"
  }

  test("weight of a larger tree") {
    new TestTrees {
      assert(weight(t0) === 2,
        "t0 = Leaf('a', 2): weight = 2")
      assert(weight(t1) === 5,
        "t1 = Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5): weight = 5")
    }
  }
  test("chars of a larger tree") {
    new TestTrees {
      assert((chars(t0) === List('a')),
        "t0 = Leaf('a', 2): chars = " + chars(t0))
      assert((chars(t2) === List('a','b', 'd')),
        "t2 = Fork(Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5), " +
          "Leaf('d',4), List('a','b','d'), 9): chars = " + chars(t2))
    }
  }

  test("string2Chars(\"hello, world\")") {
    assert(string2Chars("hello, world") ===
      List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd'))
    assert(string2Chars("xtxextx") ===
      List('x', 't', 'x', 'e', 'x', 't', 'x'),
      "\"xtxextx\": string2Chars = " + string2Chars("xtxextx"))
  }

  test("times test") {
    new TestTrees {
      assert(times(leaflist) === List(('x', 4), ('t', 2), ('e', 1)),
        "leaflist = " + leaflist + ": times = " + times(leaflist))
    }
  }

  test("makeOrderedLeafList for some frequency table") {
    new TestTrees {
      assert(makeOrderedLeafList(timesleaflist) ===
        List(Leaf('e',1), Leaf('t',2), Leaf('x',4)),
        "timesleaflist = " + timesleaflist +
          ": makeOrderedLeafList = " + makeOrderedLeafList(timesleaflist))
    }
  }

  test("singleton of some List[CodeTree]") {
    new TestTrees {
      assert(!singleton(combinelist),
        "combinelist = " + combinelist +
          "singleton: " + singleton(combinelist))
      assert(singleton(combine(combinelist)),
        "combinelist = " + combinelist +
          "singleton: " + singleton(combinelist))
    }
  }

  test("combine of some leaf list") {
    new TestTrees {
      assert(combine(makeorderedleaflist) ===
        List(Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3), Leaf('x',4)),
        "makeorderedleaflist = " + makeorderedleaflist +
          "combine = " + combine(makeorderedleaflist))
    }
  }

  test("until of codetree") {
    new TestTrees {
      assert(until(singleton, combine)(makeorderedleaflist) ===
        List(Fork(Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3),
          Leaf('x',4), List('e', 't', 'x'), 7)),
        "makeorderedleaflist = " + makeorderedleaflist +
          "until = " + until(singleton, combine)(makeorderedleaflist))
    }
  }

  test("createCodeTree") {
    new TestTrees {
      assert(createCodeTree(string2Chars(str)) ===
        Fork(Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3),
          Leaf('x',4), List('e', 't', 'x'), 7),
        "string = " + str +
          ": codeTree = " + createCodeTree(string2Chars(str)))
      assert(createCodeTree(string2Chars("someText")) ===
        Fork(
          Fork(
            Leaf('e',2),
            Fork(Leaf('s',1),Leaf('o',1),List('s', 'o'),2),
            List('e', 's', 'o'),4),
          Fork(
            Fork(Leaf('m',1),Leaf('T',1),List('m', 'T'),2),
            Fork(Leaf('x',1),Leaf('t',1),List('x', 't'),2),
            List('m', 'T', 'x', 't'),4),
          List('e', 's', 'o', 'm', 'T', 'x', 't'),8),
        "string = \"someText\": codeTree = " +
          createCodeTree(string2Chars("someText")))
    }
  }

  test("string2Ints(\"001010011\")") {
    assert(string2Ints("001010011") ===
      List(0, 0, 1, 0, 1, 0, 0, 1, 1),
      "string2Ints(\"001010011\"): " + string2Ints("001010011"))
  }

  test("decodeTree") {
    new TestTrees {
      intercept[IllegalArgumentException] {
        decode(finalCodeTree, List(0))
      }
      assert(decode(finalCodeTree, string2Ints("1")) === List('x'),
        "decodeTree: finalCodeTree " + finalCodeTree +
          ": Code 1: decode list =" +
          decode(finalCodeTree, string2Ints("1")))
      assert(decode(finalCodeTree, string2Ints("001")) === List('e', 'x'),
        "decodeTree: finalCodeTree " + finalCodeTree +
          ": Code 001: decode list =" +
          decode(finalCodeTree, string2Ints("001")))
      assert(decode(finalCodeTree, string2Ints(codeStr)) ===
        List('e', 'x', 't', 'e', 'x', 'x'),
        "decodeTree: finalCodeTree " + finalCodeTree +
          ": Code " + codeStr+ ": decode list =" +
          decode(finalCodeTree, string2Ints(codeStr)))
      assert(decodedSecret ===
        List('h', 'u', 'f', 'f', 'm', 'a', 'n', 'e', 's', 't', 'c', 'o', 'o', 'l'))
    }
  }
  test("encode") {
    new TestTrees {
      /* add a 'foreign character" ('z')that cannot be encoded" */
      intercept[IllegalArgumentException] {
        encode(finalCodeTree)("xxxtteettzxxeetxx".toList)
      }
      assert(encode(finalCodeTree)("xxxtteettxxeetxx".toList) ===
        List(1, 1, 1, 0, 1, 0, 1, 0, 0, 0, 0, 0, 1, 0, 1, 1, 1, 0, 0, 0, 0, 0, 1, 1, 1),
      "encode " + "xxxtteettxxeetxx" + "= " +
        encode(finalCodeTree)("xxxtteettxxeetxx".toList))
    }
  }
  test("quickEncode") {
    new TestTrees {
      /* add a 'foreign character" ('z')that cannot be encoded" */
      intercept[IllegalArgumentException] {
        quickEncode(finalCodeTree)("xxxtteettzxxeetxx".toList)
      }
      assert(quickEncode(finalCodeTree)("xxxtteettxxeetxx".toList) ===
        List(1, 1, 1, 0, 1, 0, 1, 0, 0, 0, 0, 0, 1, 0, 1, 1, 1, 0, 0, 0, 0, 0, 1, 1, 1),
      "quickEncode " + "xxxtteettxxeetxx" + "= " +
        quickEncode(finalCodeTree)("xxxtteettxxeetxx".toList))
    }
  }

  test("decode and encode") {
    new TestTrees {
      assert(decode(t1, encode(t1)("ab".toList)) === "ab".toList)
      assert(decode(finalCodeTree, encode(finalCodeTree)("xxxtteettxxeetxx".toList)) ===
        "xxxtteettxxeetxx".toList)
    }
  }
}
