package controllers

import play.api.libs.functional.syntax.{toFunctionalBuilderOps, unlift}
import play.api.libs.json.{JsError, JsObject, JsPath, JsValue, Json, Reads, Writes}
import play.api.mvc.{AbstractController, ControllerComponents}

import javax.inject.{Inject, Singleton}

@Singleton
class SurveyController2 @Inject() (cc: ControllerComponents) extends AbstractController(cc) {

  sealed trait Question

  implicit val questionWrites: Writes[Question] = Writes[Question] {
    case cQ: ClosedQuestion => Json.toJson(cQ)
    case oQ: OpenQuestion => Json.toJson(oQ)
  }

  implicit val questionReads: Reads[Question] = Reads[Question] {
    case json: JsObject =>
      (json \ "make").asOpt[String] match {
        //        case Some("OpenQuestion") => openQuestionReads.reads(json)
        //        case Some("ClosedQuestion") => closedQuestionReads.reads(json)
        //        case _ => JsError("Survey expected:" + json.value.size)
        case _ => if (json.value.size == 2) {
          openQuestionReads.reads(json)
        }else if (json.value.size == 3) {
          closedQuestionReads.reads(json)
        }else JsError("Survey expected:" + json)
      }
    case _ => JsError("JsObject expected")
  }

  case class OpenQuestion(var question: String, yourAnswer: String) extends Question

  implicit val openQuestionWrites: Writes[OpenQuestion] = (
    (JsPath \ "question").write[String] and
      (JsPath \ "yourAnswer").write[String]
    ) (unlift(OpenQuestion.unapply))

  implicit val openQuestionReads: Reads[OpenQuestion] = (
    (JsPath \ "question").read[String] and
      (JsPath \ "yourAnswer").read[String]
    ) (OpenQuestion.apply _)

  case class ClosedQuestion(var question: String, answers: List[String], yourAnswer: String) extends Question

  implicit val closedQuestionWrites: Writes[ClosedQuestion] = (
    (JsPath \ "question").write[String] and
      (JsPath \ "answers").write[List[String]] and
      (JsPath \ "yourAnswer").write[String]
    ) (unlift(ClosedQuestion.unapply))

  implicit val closedQuestionReads: Reads[ClosedQuestion] = (
    (JsPath \ "question").read[String] and
      (JsPath \ "answers").read[List[String]] and
      (JsPath \ "yourAnswer").read[String]
    ) (ClosedQuestion.apply _)

  case class Survey(id: Int, questions: List[Question])

  object Survey {

    var listSurvey: List[Survey] = {
      List(
        Survey(
          1,
          List(
            OpenQuestion(
              "Ktora jest godzina?",
              "Jest godzina 20:00"
            ),
            ClosedQuestion(
              "master carry",
              List("tak", "nie"),
              "tak"
            )
          )
        )
      )
    }

    def save(survey: Survey) = {
      listSurvey = listSurvey ::: List(survey)
    }


    implicit val surveyWrites: Writes[Survey] = (
      (JsPath \ "id").write[Int] and
        (JsPath \ "questions").write[List[Question]]
      ) (unlift(Survey.unapply))

    implicit val surveyReads: Reads[Survey] = (
      (JsPath \ "id").read[Int] and
        (JsPath \ "questions").read[List[Question]]
      ) (Survey.apply _)
  }

  def listSurveys = Action {
    val json = Json.toJson(Survey.listSurvey)
    //    Ok(json)
    //    Ok(views.html.survey(tasks))
    //    val result: JsValue = Json.obj("foo" -> "bar")

    Ok(views.html.survey(json.toString().substring(1, json.toString().length() -1)))
//    Ok(views.html.survey(json.toString()))

    //    Ok("ok")
  }

  def saveSurvey = Action(parse.json) { request =>
    val placeResult = request.body.validate[Survey]
    placeResult.fold(
      errors => {
        BadRequest(Json.obj("message error" -> JsError.toJson(errors)))
      },
      survey => {
        Survey.save(survey)
        Ok(Json.obj("message ok" -> (survey.questions)))
      }
    )
  }
}
