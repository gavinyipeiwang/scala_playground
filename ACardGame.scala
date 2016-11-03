object ACardGame {

  sealed trait CardColor

  case object Black extends CardColor

  case object Red extends CardColor

  case class Card(color: CardColor, value: Int)

  sealed trait Move

  case object Drawing extends Move

  case object Discarding extends Move

  case class IllegalMoveException(msg: String) extends Exception

  /*
  It takes a list of cards cs, a card c, and an exception e. It returns a
  list that has all the elements of cs except c. If c is in the list more than once, remove only the rst one.
  If c is not in the list, raise the exception e.
   */
  def remove_card(cs: List[Card], c: Card): List[Card] = cs match {
    case head :: tail => if (head == c) tail else head :: remove_card(tail, c)
    case Nil => throw new RuntimeException
  }

  /*
  It takes a list of cards and returns true if all the cards in the
  list are the same color.
   */
  def all_same_color(cs: List[Card]): Boolean = cs match {
    case h1 :: h2 :: tail => if (h1.color != h2.color) false else all_same_color(h2 :: tail)
    case h :: Nil => true
    case Nil => false
  }

  /*
  It takes a list of cards and returns the sum of their values. Use a locally
  dened helper function that is tail recursive.
   */
  def sum_cards(cs: List[Card]): Int = cs.foldLeft(0)((acc, next) => acc + next.value)

  /*
  It takes a card list (the held-cards) and an int (the goal) and computes
  the score as described above.
   */
  def score(cs: List[Card], goal: Int): Int = {
    val sum = sum_cards(cs)
    if (sum > goal) 3 * (sum - goal)
    else goal - sum
  }

  /*
  It takes a card list (the card-list) a move list
  (what the player \does" at each point), and an int (the goal) and returns the score at the end of the
  game after processing (some or all of) the moves in the move list in order. Use a locally dened recursive
  helper function that takes several arguments that together represent the current state of the game. As
  described above:
 The game starts with the held-cards being the empty list.
 The game ends if there are no more moves. (The player chose to stop since the move list is empty.)
 If the player discards some card c, play continues (i.e., make a recursive call) with the held-cards
  not having c and the card-list unchanged. If c is not in the held-cards, raise the IllegalMove
  exception.
 If the player draws and the card-list is (already) empty, the game is over. Else if drawing causes
  the sum of the held-cards to exceed the goal, the game is over (after drawing). Else play continues
  with a larger held-cards and a smaller card-list.
   */
  def officiate(cs: List[Card], goal: Int, ms: List[Move]): Int = {
    def helper(held: List[Card], cards: List[Card], msl: List[Move]): Int = msl match {
      case x :: xs => x match {
        case Discarding => {
          if (held.size == 0) throw new IllegalMoveException("Held cards are empty")
          else helper(held.drop(0), cards, xs)
        }
        case Drawing => cards match {
          case Nil => score(held, goal)
          case head :: tail => {
            if (sum_cards(held) + head.value > goal) score(held, goal)
            else helper(head :: held, tail, xs)
          }
        }
      }
      case Nil => score(held, goal)
    }
    helper(List(), cs, ms)
  }


}
