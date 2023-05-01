package ua.kpi.pti.fp.l1

import org.scalacheck.Prop

case class Assignee(
  fullName: String,
  props: List[Prop] = List(Prop.falsified),
)

object Assignee {
  val all = List(
    Assignee(
      "Ткаленко Роман Юрійович",
    ),
    Assignee("Геращенко Володимир Сергійович"),
    Assignee("Гриценко Марія Дмитрівна"),
    Assignee("Дорошенко Юрій Олександрович"),
    Assignee("Кіяшко Ігор Володимирович"),
    Assignee("Корнійчук Іван Геннадійович"),
    Assignee("Куц Дмитро Сергійович"),
    Assignee("Кушнір Влада Василівна"),
    Assignee("Лопатецький Михайло Володимирович"),
    Assignee("Мишинкін Богдан Сергійович"),
    Assignee("Недашківська Аріна Віталіївна"),
    Assignee("Прунчак Кирило Миколайович"),
    Assignee("Репінська Дарья Андріївна"),
    Assignee("Счастний Максим Валерійович"),
    Assignee("Філін Денис Дмитрович"),
    Assignee("Фурутіна Євгенія Віталіївна"),
    Assignee("Балацька Вікторія Віталіївна"),
    Assignee("Бондаренко Олександр Сергійович"),
    Assignee("Дідух Максим Андрійович"),
    Assignee("Ісаченко Нікіта Сергійович"),
    Assignee("Леськів Василина Володимирівна"),
    Assignee("Пацьора Поліна Олегівна"),
  )
}
