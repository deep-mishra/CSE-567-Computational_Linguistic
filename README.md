# CSE-567-Computational_Linguistic
Natural Language Processing Projects (implmented in Prolog)

## Roman Decimal Convertor
**Problem:** Prolog program to convert Roman to Decimal and vice-versa till 20 numbers

[RomanDecimalConversion.pl](roman-decimal-convertor/RomanDecimalConversion.pl)

Sample outputs:

![output](roman-decimal-convertor/output/output.png)

## Bigram Model Sentence Evaluator
**Description** Prolog project for application of bigram model to evaluate the correctness of english sentence.

The project constructs a Prolog bigram language model from small [DA_Corpus.text](bigram-sentense-evaluator/DA_Corpus.txt) corpus.

In order to achieve this, the corpus is first normalized and a prolog readable [unigram.pl](bigram-sentense-evaluator/unigrams.pl) and [bigram.pl](bigram-sentense-evaluator/bigrams.pl) database is created. In the final step, the [bigram_model.pl](bigram-sentense-evaluator/bigram_model.pl) computes the probability of any word sequence, of any size, via a predicate called **calc_prob/2**. The predicate calc_prob/2 works in log space and applies laplace smoothing on fly to compute the probability of given sentence.
