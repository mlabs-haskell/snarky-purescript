import { AccountUpdate, TokenId } from './account_update.js';
import * as Mina from './mina.js';
import { fetchAccount } from './fetch.js';
import { Provable } from './provable.js';
import { Field } from '../lib/core.js';
// external API
export { State, state, declareState };
// internal API
export { assertStatePrecondition, cleanStatePrecondition };
function State() {
    return createState();
}
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
function state(stateType) {
    return function (target, key, _descriptor) {
        const ZkappClass = target.constructor;
        if (reservedPropNames.has(key)) {
            throw Error(`Property name ${key} is reserved.`);
        }
        let sc = smartContracts.get(ZkappClass);
        if (sc === undefined) {
            sc = { states: [], layout: undefined };
            smartContracts.set(ZkappClass, sc);
        }
        sc.states.push([key, stateType]);
        Object.defineProperty(target, key, {
            get() {
                return this._?.[key];
            },
            set(v) {
                if (v._contract !== undefined)
                    throw Error('A State should only be assigned once to a SmartContract');
                if (this._?.[key])
                    throw Error('A @state should only be assigned once');
                v._contract = {
                    key,
                    stateType: stateType,
                    instance: this,
                    class: ZkappClass,
                    wasConstrained: false,
                    wasRead: false,
                    cachedVariable: undefined,
                };
                (this._ ?? (this._ = {}))[key] = v;
            },
        });
    };
}
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
function declareState(SmartContract, states) {
    for (let key in states) {
        let CircuitValue = states[key];
        state(CircuitValue)(SmartContract.prototype, key);
    }
}
function createState() {
    return {
        _contract: undefined,
        set(state) {
            if (this._contract === undefined)
                throw Error('set can only be called when the State is assigned to a SmartContract @state.');
            let layout = getLayoutPosition(this._contract);
            let stateAsFields = this._contract.stateType.toFields(state);
            let accountUpdate = this._contract.instance.self;
            stateAsFields.forEach((x, i) => {
                AccountUpdate.setValue(accountUpdate.body.update.appState[layout.offset + i], x);
            });
        },
        assertEquals(state) {
            if (this._contract === undefined)
                throw Error('assertEquals can only be called when the State is assigned to a SmartContract @state.');
            let layout = getLayoutPosition(this._contract);
            let stateAsFields = this._contract.stateType.toFields(state);
            let accountUpdate = this._contract.instance.self;
            stateAsFields.forEach((x, i) => {
                AccountUpdate.assertEquals(accountUpdate.body.preconditions.account.state[layout.offset + i], x);
            });
            this._contract.wasConstrained = true;
        },
        assertNothing() {
            if (this._contract === undefined)
                throw Error('assertNothing can only be called when the State is assigned to a SmartContract @state.');
            this._contract.wasConstrained = true;
        },
        get() {
            if (this._contract === undefined)
                throw Error('get can only be called when the State is assigned to a SmartContract @state.');
            // inside the circuit, we have to cache variables, so there's only one unique variable per on-chain state.
            // if we'd return a fresh variable everytime, developers could easily end up linking just *one* of them to the precondition,
            // while using an unconstrained variable elsewhere, which would create a loophole in the proof.
            if (this._contract.cachedVariable !== undefined &&
                // `inCheckedComputation() === true` here always implies being inside a wrapped smart contract method,
                // which will ensure that the cache is cleaned up before & after each method run.
                Provable.inCheckedComputation()) {
                this._contract.wasRead = true;
                return this._contract.cachedVariable;
            }
            let layout = getLayoutPosition(this._contract);
            let contract = this._contract;
            let inProver_ = Provable.inProver();
            let stateFieldsType = Provable.Array(Field, layout.length);
            let stateAsFields = Provable.witness(stateFieldsType, () => {
                let account;
                try {
                    account = Mina.getAccount(contract.instance.address, contract.instance.self.body.tokenId);
                }
                catch (err) {
                    // TODO: there should also be a reasonable error here
                    if (inProver_) {
                        throw err;
                    }
                    throw Error(`${contract.key}.get() failed, either:\n` +
                        `1. We can't find this zkapp account in the ledger\n` +
                        `2. Because the zkapp account was not found in the cache. ` +
                        `Try calling \`await fetchAccount(zkappAddress)\` first.\n` +
                        `If none of these are the case, then please reach out on Discord at #zkapp-developers and/or open an issue to tell us!`);
                }
                if (account.zkapp?.appState === undefined) {
                    // if the account is not a zkapp account, let the default state be all zeroes
                    return Array(layout.length).fill(Field(0));
                }
                else {
                    let stateAsFields = [];
                    for (let i = 0; i < layout.length; ++i) {
                        stateAsFields.push(account.zkapp.appState[layout.offset + i]);
                    }
                    return stateAsFields;
                }
            });
            let state = this._contract.stateType.fromFields(stateAsFields);
            if (Provable.inCheckedComputation())
                this._contract.stateType.check?.(state);
            this._contract.wasRead = true;
            this._contract.cachedVariable = state;
            return state;
        },
        getAndAssertEquals() {
            let state = this.get();
            this.assertEquals(state);
            return state;
        },
        async fetch() {
            if (this._contract === undefined)
                throw Error('fetch can only be called when the State is assigned to a SmartContract @state.');
            if (Mina.currentTransaction.has())
                throw Error('fetch is not intended to be called inside a transaction block.');
            let layout = getLayoutPosition(this._contract);
            let address = this._contract.instance.address;
            let { account } = await fetchAccount({
                publicKey: address,
                tokenId: TokenId.toBase58(TokenId.default),
            });
            if (account === undefined)
                return undefined;
            let stateAsFields;
            if (account.zkapp?.appState === undefined) {
                stateAsFields = Array(layout.length).fill(Field(0));
            }
            else {
                stateAsFields = [];
                for (let i = 0; i < layout.length; i++) {
                    stateAsFields.push(account.zkapp.appState[layout.offset + i]);
                }
            }
            return this._contract.stateType.fromFields(stateAsFields);
        },
        fromAppState(appState) {
            if (this._contract === undefined)
                throw Error('fromAppState() can only be called when the State is assigned to a SmartContract @state.');
            let layout = getLayoutPosition(this._contract);
            let stateAsFields = [];
            for (let i = 0; i < layout.length; ++i) {
                stateAsFields.push(appState[layout.offset + i]);
            }
            return this._contract.stateType.fromFields(stateAsFields);
        },
    };
}
function getLayoutPosition({ key, class: contractClass, }) {
    let layout = getLayout(contractClass);
    let stateLayout = layout.get(key);
    if (stateLayout === undefined) {
        throw new Error(`state ${key} not found`);
    }
    return stateLayout;
}
function getLayout(scClass) {
    let sc = smartContracts.get(scClass);
    if (sc === undefined)
        throw Error('bug');
    if (sc.layout === undefined) {
        let layout = new Map();
        sc.layout = layout;
        let offset = 0;
        sc.states.forEach(([key, stateType]) => {
            let length = stateType.sizeInFields();
            layout.set(key, { offset, length });
            offset += length;
        });
    }
    return sc.layout;
}
// per-smart contract class context for keeping track of state layout
const smartContracts = new WeakMap();
const reservedPropNames = new Set(['_methods', '_']);
function assertStatePrecondition(sc) {
    try {
        for (let [key, context] of getStateContexts(sc)) {
            // check if every state that was read was also contrained
            if (!context?.wasRead || context.wasConstrained)
                continue;
            // we accessed a precondition field but not constrained it explicitly - throw an error
            let errorMessage = `You used \`this.${key}.get()\` without adding a precondition that links it to the actual on-chain state.
Consider adding this line to your code:
this.${key}.assertEquals(this.${key}.get());`;
            throw Error(errorMessage);
        }
    }
    finally {
        cleanStatePrecondition(sc);
    }
}
function cleanStatePrecondition(sc) {
    for (let [, context] of getStateContexts(sc)) {
        if (context === undefined)
            continue;
        context.wasRead = false;
        context.wasConstrained = false;
        context.cachedVariable = undefined;
    }
}
function getStateContexts(sc) {
    let scClass = sc.constructor;
    let scInfo = smartContracts.get(scClass);
    if (scInfo === undefined)
        return [];
    return scInfo.states.map(([key]) => [key, sc[key]?._contract]);
}
//# sourceMappingURL=state.js.map