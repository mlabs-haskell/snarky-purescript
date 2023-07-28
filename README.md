## WTF? 

This is an extremely experimental PureScript embedding of [SnarkyJS](https://github.com/o1-labs/snarkyjs/tree/main). Its official purpose is a proof-of-concept for a DSL in support of the MLabs (zkPal)[https://zkignite.minaprotocol.com/zkignite/dev4dev/draftproposals/suggestion/569] zkIgnite proposal. Its unofficial purpose is facilitate learning about SnarkyJS/zkCircuits and to do some fun things with types :)

If you're from zkIgnite, please note that this is a research project intended to demonstrate the viability of the concept of a circuit DSL. It does not represent the final design. Though we are rather fond of Purescript, we do not want to force people to learn the language if we can avoid it.

At the moment, it's capable of generating zkCircuits and proofs of things like `true == true`. Getting to that point was the hard part though! Ergo you can expect features and stability to increase fairly quickly. 

## How to use 

  1. Write a function `myFunc:: public -> private -> Context Void`, where `public` and `private` are instances of the `SnarkyPS.Lib.CircuitValue.CircuitValue` class. 
      - A `CircuitValue` is either a primitive `FieldLike` type (for now just `Bool` and `U64`, can be extended later), a *record of primitive circuit types*, or a *record of `CircuitValue`s*. 
      - `Context Void` is the return type of all Assertions. (Note to self, rename `Void` to `Assertion` and `Context` to something more informative). 
      - `Context` is a `Monad`, so you can use `do` notation if you want. Morally it is the "monad half" of `Identity` without its "comonad half". That sounds scary, but all it really means is there is *no way to extract the `a` from a `Context a`*. 
      - ATM you can't write a very *interesting* function, but you will be able to soon :D 
  2. Compile your function using `SnarkyPS.Lib.Circuit.mkCircuit`. This gives you a `Circuit public private`. 
  3. Run your circuit to (hopefully!) generate a `Proof public private` by using `SnarkyPS.Lib.Circuit.prove`. This has the type `Circuit a b -> a -> b -> Aff (Proof a b)`, so if you have a `Circuit Bool U64` you would run it like: `prove myFunc (bool true) (u64 lol)`. 
      - If you're a JS programmer, you can think of `bool` and `u64` as constructors like `BigInt` (or w/e). 
      - `Aff` is PureScript for `Promise` (more or less). See `Main` for an example. 
      
## NOTES: 

- **EXTREMELY EXPERIMENTAL, TOTALLY UNTESTED, DO NOT USE OR THINK ABOUT USING IN ANYTHING NEAR PRODUCTION**. By using this you agree that I am not at fault if you foolishly trust this and lose a bunch of money or blow yourself up or something. 
- Still need to remove some hardcoded traces
- Can't do much with this yet because it does not yet have: 
  - Record field accessors that you can use in a `Context`. If you try to use the normal ones, SnarkyJS will probably yell at you (tbh haven't tried it, maybe it'd work? that'd be cool) 
  - Control flow, e.g. `if/switch/etc`. This is really easy to implement, but I just managed to get it to prove that `true == true` half an hour ago, so gimme a bit :P 
  - Non-broken bindings. Didn't realize I had to use `Function.prototype.bind()` everywhere, will fix shortly 
  - Any tests at all. Seriously, don't use this for anything real just yet! The only thing you can really rely on is the typeclass magic in `SnarkyPS.Lib.CircuitValue` (because types are logic and there aren't any surprises in logic)
  - This was written *extremely quickly* and therefore has a horrible repository structure and no build/dependency tooling, e.g. I just dumped the `dist` dir from SnarkyJS build output into the `snarkydist` directory. This will also change sooner than later.  
  

