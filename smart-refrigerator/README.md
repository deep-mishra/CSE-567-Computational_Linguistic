
##
## Smart Refrigerator - A Natural language interface for fridge
**Problem:** Develop a Natural Language interface for Fridge. The interface should be capable for parsing english sentence, evaluating the data from data model (mini database), and respond to user query appropriately.

[Smart Refrigerator - code](code)

**Approach:**
  The project is divided into three sub modules namely Parsing, ModelChecker, and Response.
  
  **Parsing** module applies First Order Logic on tokenized input string to create the formula for given sentence. It does this by applying lexicons and rules of english grammer. The module uses augmented version of SR Parser (Shift Reduce Parser) to parse the sentence.
  
  **ModelChecker** evaluate the output of Parser using model data (Prolog database for fridge). It identifies if the sentence was declarative, interrogative or content question.  
  
  **Response** module prints the result of ModelChecker based on response type.
