package suggestions
package gui

import scala.collection.mutable.ListBuffer
import scala.collection.JavaConverters._
import scala.concurrent._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.swing._
import scala.util.{ Try, Success, Failure }
import scala.swing.event._
import swing.Swing._
import javax.swing.UIManager
import Orientation._
import rx.subscriptions.CompositeSubscription
import rx.lang.scala.Observable
import rx.lang.scala.Subscription
import observablex._
import search._

object WikipediaSuggest extends SimpleSwingApplication with ConcreteSwingApi with ConcreteWikipediaApi {

  {
    try {
      UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName())
    } catch {
      case t: Throwable =>
    }
  }

  /*
   * wait time for HTTP requests fail
   */ 
  val maxWaitTimeHTTPGetInSecs: Long = 5

  def searchAction(button: Button, searchTermField: TextField,
    suggestionList: ListView[String], status: Label,
    editorpane: EditorPane): Unit = {
    val eventScheduler = SchedulerEx.SwingEventThreadScheduler

    val searchTerms: Observable[String] = {
      /*
       * - textValues maps the text entered in the textField
       *   (searchTermField) to Observable[String]
       * - sanitized replaces as blank spaces with underscore as
       *   search terms sent in an HTTP request cannot contain spaces
       */
      searchTermField.textValues.sanitized
    }

    val suggestions: Observable[Try[List[String]]] = {
      /*
       * - wait bounded time in case HTTP GET request fails
       *   for suggestion Lists
       * - wrap result in Try as we keep the throwable to
       *   print the error message: use concatRecovered
       */
      searchTerms.concatRecovered(
        term => wikiSuggestResponseStream(term).
          timedOut(maxWaitTimeHTTPGetInSecs))
    }

    val suggestionSubscription: Subscription =
      suggestions.observeOn(eventScheduler) subscribe {
        (t: Try[List[String]]) => t match {
          case Success(listString) => suggestionList.listData = listString
          case Failure(ex) => status.text = ex.toString
        }
      }

    /*
     * - If the suggestion list had no items
     *   selected, then the click should not be part of selections.
     * - suggestionList.selection.items return a list of
     *   selected items and not just one single item.
     *   in this case we return only one item in list
     */
    val selections: Observable[String] = {
      val obsButton: Observable[Button] = button.clicks
      obsButton.filter(b => !suggestionList.selection.items.isEmpty).
        map(b => suggestionList.selection.items.head)
    }

    /*
     * - Using selections observable we obtain an observable
     *   of the Wikipedia pages corresponding to the respective
     *   search term. Requests may fail: wrap them into Try.
     * - wait bounded time in case HTTP GET request fails
     *   for the suggestion entry selected.
     */
    val pages: Observable[Try[String]] = {
      selections.concatRecovered(
        term => wikiPageResponseStream(term).
          timedOut(maxWaitTimeHTTPGetInSecs))
    }

    /*
     * - Render the observable pages: Subscribe to the
     *   pages observable to update the editorpane with the
     *   contents of the response.
     */
    val pageSubscription: Subscription =
      pages.observeOn(eventScheduler) subscribe {
        (tryPage: Try[String]) => tryPage match {
          case Success(page) => editorpane.text = page
          case Failure(ex) => editorpane.text =
            s"HTTP Fetch page failed with exception ${ex.toString}"
        }
      }
  }

  def top = new MainFrame {
    /* gui setup */
    title = "Query Wikipedia"
    minimumSize = new Dimension(900, 600)

    val button = new Button("Get") {
      icon = new javax.swing.ImageIcon(javax.imageio.ImageIO.read(this.getClass.getResourceAsStream("/suggestions/wiki-icon.png")))
    }
    val searchTermField = new TextField
    val suggestionList = new ListView(ListBuffer[String]())
    val status = new Label(" ")
    val editorpane = new EditorPane {
      import javax.swing.border._
      border = new EtchedBorder(EtchedBorder.LOWERED)
      editable = false
      peer.setContentType("text/html")
    }

    contents = new BoxPanel(orientation = Vertical) {
      border = EmptyBorder(top = 5, left = 5, bottom = 5, right = 5)
      contents += new BoxPanel(orientation = Horizontal) {
        contents += new BoxPanel(orientation = Vertical) {
          maximumSize = new Dimension(240, 900)
          border = EmptyBorder(top = 10, left = 10, bottom = 10, right = 10)
          contents += new BoxPanel(orientation = Horizontal) {
            maximumSize = new Dimension(640, 30)
            border = EmptyBorder(top = 5, left = 0, bottom = 5, right = 0)
            contents += searchTermField
          }
          contents += new ScrollPane(suggestionList)
          contents += new BorderPanel {
            maximumSize = new Dimension(640, 30)
            add(button, BorderPanel.Position.Center)
          }
        }
        contents += new ScrollPane(editorpane)
      }
      contents += status
    }

    searchAction(button, searchTermField, suggestionList,
      status, editorpane)
  }
}

trait ConcreteWikipediaApi extends WikipediaApi {
  def wikipediaSuggestion(term: String) = Search.wikipediaSuggestion(term)
  def wikipediaPage(term: String) = Search.wikipediaPage(term)
}


trait ConcreteSwingApi extends SwingApi {
  type ValueChanged = scala.swing.event.ValueChanged
  object ValueChanged {
    def unapply(x: Event) = x match {
      case vc: ValueChanged => Some(vc.source.asInstanceOf[TextField])
      case _ => None
    }
  }
  type ButtonClicked = scala.swing.event.ButtonClicked
  object ButtonClicked {
    def unapply(x: Event) = x match {
      case bc: ButtonClicked => Some(bc.source.asInstanceOf[Button])
      case _ => None
    }
  }
  type TextField = scala.swing.TextField
  type Button = scala.swing.Button
}
