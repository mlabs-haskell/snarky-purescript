import { FlexibleProvablePure } from './circuit_value.js';
import { SmartContract } from './zkapp.js';
import { Field } from '../lib/core.js';
export { State, state, declareState };
export { assertStatePrecondition, cleanStatePrecondition };
/**
 * Gettable and settable state that can be checked for equality.
 */
type State<A> = {
    /**
     * Get the current on-chain state.
     *
     * Caution: If you use this method alone inside a smart contract, it does not prove that your contract uses the current on-chain state.
     * To successfully prove that your contract uses the current on-chain state, you must add an additional `.assertEquals()` statement or use `.getAndAssertEquals()`:
     *
     * ```ts
     * let x = this.x.get();
     * this.x.assertEquals(x);
     * ```
     *
     * OR
     *
     * ```ts
     * let x = this.x.getAndAssertEquals();
     * ```
     */
    get(): A;
    /**
     * Get the current on-chain state and prove it really has to equal the on-chain state,
     * by adding a precondition which the verifying Mina node will check before accepting this transaction.
     */
    getAndAssertEquals(): A;
    /**
     * Set the on-chain state to a new value.
     */
    set(a: A): void;
    /**
     * Asynchronously fetch the on-chain state. This is intended for getting the state outside a smart contract.
     */
    fetch(): Promise<A | undefined>;
    /**
     * Prove that the on-chain state has to equal the given state,
     * by adding a precondition which the verifying Mina node will check before accepting this transaction.
     */
    assertEquals(a: A): void;
    /**
     * **DANGER ZONE**: Override the error message that warns you when you use `.get()` without adding a precondition.
     */
    assertNothing(): void;
    /**
     * Get the state from the raw list of field elements on a zkApp account, for example:
     *
     * ```ts
     * let myContract = new MyContract(address);
     * let account = Mina.getAccount(address);
     *
     * let x = myContract.x.fromAppState(account.zkapp!.appState);
     * ```
     */
    fromAppState(appState: Field[]): A;
};
declare function State<A>(): State<A>;
/**
 * A decorator to use within a zkapp to indicate what will be stored on-chain.
 * For example, if you want to store a field element `some_state` in a zkapp,
 * you can use the following in the declaration of your zkapp:
 *
 * ```
 * @state(Field) some_state = State<Field>();
 * ```
 *
 */
declare function state<A>(stateType: FlexibleProvablePure<A>): (target: SmartContract & {
    constructor: any;
}, key: string, _descriptor?: PropertyDescriptor) => void;
/**
 * `declareState` can be used in place of the `@state` decorator to declare on-chain state on a SmartContract.
 * It should be placed _after_ the class declaration.
 * Here is an example of declaring a state property `x` of type `Field`.
 * ```ts
 * class MyContract extends SmartContract {
 *   x = State<Field>();
 *   // ...
 * }
 * declareState(MyContract, { x: Field });
 * ```
 *
 * If you're using pure JS, it's _not_ possible to use the built-in class field syntax,
 * i.e. the following will _not_ work:
 *
 * ```js
 * // THIS IS WRONG IN JS!
 * class MyContract extends SmartContract {
 *   x = State();
 * }
 * declareState(MyContract, { x: Field });
 * ```
 *
 * Instead, add a constructor where you assign the property:
 * ```js
 * class MyContract extends SmartContract {
 *   constructor(x) {
 *     super();
 *     this.x = State();
 *   }
 * }
 * declareState(MyContract, { x: Field });
 * ```
 */
declare function declareState<T extends typeof SmartContract>(SmartContract: T, states: Record<string, FlexibleProvablePure<unknown>>): void;
declare function assertStatePrecondition(sc: SmartContract): void;
declare function cleanStatePrecondition(sc: SmartContract): void;
