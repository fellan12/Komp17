import scala.collection.mutable.ListBuffer


object Main {

	def main(args: Array[String]) {
		//Exercise 1
		/*
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + "")
      println()
    }
    *
    */

		//Exercise 2
		/*
    val test = "asdfb()sdg)asd(asdaga".toList   //Ska ge false
    println(balance(test))
    val test2 = "asd()ag(asdg)".toList
    println(balance(test2))            //Ska ge true
		 */

		//Exercise 3
		val coins = List(1,2,3)
		println(countChange(5, coins)) 
		
	}

	/**
	 * Exercise 1
	 */
	def pascal(c: Int, r: Int): Int = {
			if (c == 0 || c == r){
				return 1 
			}else{
				return pascal(c - 1, r - 1) + pascal(c, r - 1)
			}    
	}


	/**
	 * Exercise 2
	 */
	def balance(chars: List[Char]): Boolean = {    
			def recursive(chars: List[Char], opens: Int): Boolean = {
					var open: Int  = opens

					if(chars.isEmpty){
						return open == 0
					}else{ 
						if(chars.head == '('){
							open += 1
						}else if (chars.head == ')'){
							open -= 1
						}

						if(opens >= 0){
							return recursive(chars.tail, open) 
						}else{
							return false;
						}

					}     
			}

			return recursive(chars, 0)
	}

	/**
	 * Exercise 3
	 */
	def countChange(money: Int, coins: List[Int]): Int = {
	  	money match {
	    	case 0  => 1
	    	case m if m < 0 => 0
	      	case m if m > 0 && coins.isEmpty => 0
	    	case _ => countChange(money, coins.tail) + countChange(money - coins.head, coins)
	  	}
	}
   
}