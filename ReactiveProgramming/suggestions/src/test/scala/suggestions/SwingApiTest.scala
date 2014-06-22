package suggestions



import scala.collection._
import scala.concurrent._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{Try, Success, Failure}
import scala.swing.event.Event
import scala.swing.Reactions.Reaction
import rx.lang.scala._
import org.scalatest._
import gui._

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class SwingApiTest extends FunSuite {

  object swingApi extends SwingApi {
    class ValueChanged(val textField: TextField) extends Event

    object ValueChanged {
      def unapply(x: Event) = x match {
        case vc: ValueChanged => Some(vc.textField)
        case _ => None
      }
    }

    class ButtonClicked(val source: Button) extends Event

    object ButtonClicked {
      def unapply(x: Event) = x match {
        case bc: ButtonClicked => Some(bc.source)
        case _ => None
      }
    }

    class Component {
      private val subscriptions = mutable.Set[Reaction]()
      def subscribe(r: Reaction) {
        subscriptions add r
      }
      def unsubscribe(r: Reaction) {
        subscriptions remove r
      }
      def publish(e: Event) {
        for (r <- subscriptions) r(e)
      }
    }

    class TextField extends Component {
      private var _text = ""
      def text = _text
      def text_=(t: String) {
        _text = t
        publish(new ValueChanged(this))
      }
    }

    class Button extends Component {
      def click() {
        publish(new ButtonClicked(this))
      }
    }
  }

  import swingApi._

  test("SwingApi: emit text values to the observable while subscribed") {
    val textField = new swingApi.TextField
    val values: Observable[String] = textField.textValues

    val observed = mutable.Buffer[String]()

    // write some text now
    textField.text = "T"
    textField.text = "Tu"

    val sub = values subscribe {
      observed += _
    }

    textField.text = "Tur"
    textField.text = "Turi"
    textField.text = "Turin"
    textField.text = "Turing"

    // unsubscribe
    sub.unsubscribe()
    textField.text = "Turing "
    textField.text = "Turing Award"

    assert(observed === Seq("Tur", "Turi", "Turin", "Turing"),
      observed)
  }

  test("SwingApi: emit Button Clicks to the observable while subscribed") {
    val button = new swingApi.Button
    val buttonClickObs: Observable[Button] = button.clicks

    var numClicks = 0

    // click button few times
    button.click(); button.click();
    val sub = buttonClickObs.subscribe{(b: Button) => numClicks += 1}
    
    button.click(); button.click(); button.click()
    sub.unsubscribe()
    button.click();

    assert(numClicks === 3, numClicks)
  }
}
