Introduction
----

Lina is an embedded, in Haskell, domain specific programming language for conducting threat analysis of both physical and virtual secure systems.  Currently, Lina only supports threat analysis using Attack Trees, but we are in the early stages of development and have plans to add a number of other threat analysis models; see below.

A primary feature of Lina's is ease of use, that is, we want the average security practitioner to be able to make use of Lina with as little experience with functional programming or Haskell as possible.  Thus, over time, we will be building additional tooling to make this easier.  A second hope for Lina is that it can be used as a target for new threat analysis tools especially automated generation tools.

Before showing off some of the features of Lina the reader might be wondering, why a programming language? In addition, why functional, why Haskell?

First, it is quite easy to see that a programming language for specifying models is needed, because largely, in practice the models are defined in a pseudo-scripting fashion, and so it begs the question, why are we the first ones to do this?

Second, as security researchers and practitioners it is fair to say that we consider correctness to be of the utmost importance, and thus, by embedding Lina in Haskell we get to absorb all of its features, and hence, its higher degree of confidence when thinking about correctness.  Haskell is a statically-typed functional programming language whose advanced typing features can be exploited by Lina to offer a higher degree of confidence to the programmer that other languages cannot.  In addition, we can take advantage of cutting edge verification techniques like property based testing using QuickCheck as well as refinement types in Liquid Haskell.

Third, as a programming language we can easily target multiple different automated reasoning tools at once, and easily extend to others.  For example, we currently support using Maude specifications to prove properties between attack trees; see below.

Consider the following simple example of an ATM attack:

```.(haskell)
atm :: PAttackTree String
atm = start_PAT $
  seq_node "ATM attack"
    (and_node "get credentials"
       (base_na "steal card")
       (or_node "get PIN"
          (base_na "social engineer")
          (base_na "find a post-it")))
    (base_na "withdraw money")
```
This is an attack tree without attributes on the base attacks which we call a Process Attack Tree (PAttackTree).  Anyone familiar with attack trees can easily understand the previous definition, and the syntax largely stays the same for attributed attack trees.  Consider a second example:

```.(haskell)
apat :: APAttackTree Integer String
apat = start_PAT $
  or_node "or 1"
    (and_node "and 1"
       (or_node "or 2"
          (base_wa 1 "base 1")
          (base_wa 2 "base 2"))
       (base_wa 3 "base 3"))
    (seq_node "seq 1"
       (base_wa 6 "base 4")
       (or_node "or 3"
          (base_wa 5 "base 5")
          (base_wa 6 "base 6")))
```
This example does have attributes on its base attacks, but as we can see, the syntax is essentially the same as above.

The previous example can then be turned into a full attack tree as follows:

```.(haskell)
at :: Conf Integer -> AttackTree Integer String
at conf = start_AT conf (insert pat1)
```
The attack tree `at` is parameterized by a configuration which specifies how the attibutes on OR-nodes, AND-nodes, and SEQ-nodes should be computed.  Configurations have the following definition:

```.(haskell)
data Conf attribute = Ord attribute => Conf {
      orOp  :: attribute -> attribute -> attribute,
      andOp :: attribute -> attribute -> attribute,
      seqOp :: attribute -> attribute -> attribute
}
```
Then we can specify an example configuration as follows:

```.(haskell)
minAddMulConf :: (Ord a,Semiring a) => Conf a
minAddMulConf = Conf min (.+.) (.*.)
```
Now if we apply `at` to `addMulConf`, as in `at addMulConf`, we obtain an attack tree whose OR-nodes will be assigned the minimum attribute between its children, AND-nodes will be assigned the attribute that is computed by taking the sum of its children, and whose SEQ-nodes will be assigned the attribute that is computed by taking the product of its children.

Configurations make it possible to define the basic structure of an attack tree, and then be able to conduct several different types of analysis by inserting different configurations.  In addition, configurations and all of our different notions of attack trees are completely abstract, meaning, the labels and attributes can be any data type as long as the labels are comparable and the attribute type forms a semiring.  This makes Lina one of the most flexible threat analysis tools in existence.

We can now make several types of queries against an attack tree:
  - ask for the set of possible attacks,
  - compute the most likely attack, and
  - compute the least likely attack.

Automated Reasoning about Attack Trees using Maude
--------------------------------------------------

Consider the following second example of an ATM attack:

```.(haskell)
atm' :: PAttackTree String
atm' = start_PAT $
  or_node "ATM attack"
    (seq_node "attack vector 1"
       (and_node "get credentials 1"
          (base_na "social engineer")
          (base_na "steal card"))
       (base_na "withdraw money"))
    (seq_node "attack vector 2"
       (and_node "get credentials 2"
          (base_na "steal card")
          (base_na "find a post-it"))
       (base_na "withdraw money"))
```

This attack tree is actually equivalent to the attack tree `atm` from above.  Lina can automatically prove this, yes prove, using its [Maude](http://maude.cs.illinois.edu/w/index.php?title=The_Maude_System) backend and its formal specification of the equational rules for attack trees with sequential composition.  For example, running, `eq_PAT atm atm'` will return `True`.  

Plans for the Future
--------------------

There are lot of plans for future extensions like the following:
  - support other types of models like:
     - Attack-Defense trees,
     - Attack(-Defense) Graphs, and
     - Attack Nets.
  - different ways to view attack trees
  - support other backends:
     - other Maude backends:
        - MAV [1]
     - SMT

Installation
------------

We have tried to make installation as easy as possible.  Lina has currently only been tested on Mac OS X 10.11.6.

Please follow the following directions:

- Install [Maude](http://maude.cs.illinois.edu/w/index.php?title=The_Maude_System)
  - Make sure that the `maude` command is in your `PATH`, and the `MAUDE_LIB` environment variable is set and includes the path to the `prelude.maude` file.
- Install [stack](https://docs.haskellstack.org/en/stable/README/)
- Then run the following: 
- `git clone git@github.com:MonoidalAttackTrees/Lina.git`
-  Add the absolute path to the directory `Lina/source/Maude/maude-modules/` to  the `MAUDE_LIB` environment variable.

   For example, suppose you cloned `Lina` in the directory `/home/username/tools/Lina`, then you would add the path `/home/username/tools/Lina/source/Maude/maude-modules/` to the `MAUDE_LIB` environment variable.

- Move into the Lina directory: `cd Lina`

- Build Lina: `stack build`

   At this point `stack` will download Haskell and all of Lina's dependencies within the Lina directory and not on your system.  This means that if you want to uninstall Lina and Haskell, then simply delete the Lina directory.

- Lina is now ready to be used.


References
----------

[1] [Semantics for specialising attack trees based on linear logic](http://orbilu.uni.lu/handle/10993/34365)test
test
