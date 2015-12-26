import akka.actor.ActorSystem
import akka.stream.ActorMaterializer
import akka.stream.scaladsl.{Sink, Source}
import twitterModel.{Hashtag, Author, Tweet}


object myModule {
  implicit val system = ActorSystem("reactive-tweets")
  implicit val materializer = ActorMaterializer()

  val akka: Hashtag = Hashtag("akka")

//  val tweets: Source[Tweet, Unit]

//  val authors: Source[Author, Unit] =
//    tweets
//      .filter(_.hashtags.contains(akka))
//      .map(_.author)
//
//  authors.runWith(Sink.foreach(println))
  //authors.runForeach(println)
}

