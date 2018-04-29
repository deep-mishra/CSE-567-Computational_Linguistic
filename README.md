# CSE-567-Computational_Linguistic
Natural Language Processing programs and projects (implmented in Prolog)

## Project - Bigram Model Sentence Evaluator
**Problem:** Prolog project for application of bigram model to evaluate the correctness of english sentence.

**Approach:**
The project constructs a Prolog bigram language model using small [DA_Corpus.text](bigram-sentense-evaluator/DA_Corpus.txt) corpus.

Steps taken:

1. The [DA_Corpus.text](bigram-sentense-evaluator/DA_Corpus.txt) corpus is normalized using [unix](bigram-sentense-evaluator/unix_commands.txt) commands.
2. Created a prolog readable [unigram.pl](bigram-sentense-evaluator/unigrams.pl) and [bigram.pl](bigram-sentense-evaluator/bigrams.pl) database from normalized corpus.
3. In the final step, implemented [bigram_model.pl](bigram-sentense-evaluator/bigram_model.pl) which computes the probability of any word sequence, of any size, via a predicate called **calc_prob/2**. The predicate calc_prob/2 works in log space and applies laplace smoothing on fly to compute the probability of given sentence.

##

## Program - Roman Decimal Convertor
**Problem:** Prolog program to convert Roman to Decimal and vice-versa till 20 numbers

[RomanDecimalConversion.pl](roman-decimal-convertor/RomanDecimalConversion.pl)

**Sample outputs**:

![output](roman-decimal-convertor/output/output.png)


