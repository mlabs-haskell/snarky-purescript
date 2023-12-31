import { FlexibleProvable } from './circuit_value.js';
import { Field, Bool } from './core.js';
import { Pickles } from '../snarky.js';
import { Types } from '../bindings/mina-transaction/types.js';
import { PrivateKey, PublicKey } from './signature.js';
import { UInt64, UInt32, Int64, Sign } from './int.js';
import { SmartContract } from './zkapp.js';
import * as Precondition from './precondition.js';
import { Empty, Proof } from './proof_system.js';
import { Events, Actions } from '../bindings/mina-transaction/transaction-leaves.js';
import { Context } from './global-context.js';
export { AccountUpdate, Permissions, ZkappPublicInput };
export { smartContractContext, SetOrKeep, Permission, Preconditions, Body, Authorization, FeePayerUnsigned, ZkappCommand, addMissingSignatures, addMissingProofs, ZkappStateLength, Events, Actions, TokenId, Token, CallForest, createChildAccountUpdate, AccountUpdatesLayout, zkAppProver, SmartContractContext, dummySignature, };
declare const ZkappStateLength = 8;
type SmartContractContext = {
    this: SmartContract;
    methodCallDepth: number;
    selfUpdate: AccountUpdate;
};
declare let smartContractContext: Context.t<SmartContractContext | null>;
type ZkappProverData = {
    transaction: ZkappCommand;
    accountUpdate: AccountUpdate;
    index: number;
};
declare let zkAppProver: {
    run<Result>(witnesses: unknown[], proverData: ZkappProverData, callback: () => Promise<Result>): Promise<Result>;
    getData(): ZkappProverData;
};
type AuthRequired = Types.Json.AuthRequired;
type AccountUpdateBody = Types.AccountUpdate['body'];
type Update = AccountUpdateBody['update'];
type MayUseToken = AccountUpdateBody['mayUseToken'];
/**
 * Preconditions for the network and accounts
 */
type Preconditions = AccountUpdateBody['preconditions'];
/**
 * Either set a value or keep it the same.
 */
type SetOrKeep<T> = {
    isSome: Bool;
    value: T;
};
/**
 * One specific permission value.
 *
 * A {@link Permission} tells one specific permission for our zkapp how it
 * should behave when presented with requested modifications.
 *
 * Use static factory methods on this class to use a specific behavior. See
 * documentation on those methods to learn more.
 */
type Permission = Types.AuthRequired;
declare let Permission: {
    /**
     * Modification is impossible.
     */
    impossible: () => Permission;
    /**
     * Modification is always permitted
     */
    none: () => Permission;
    /**
     * Modification is permitted by zkapp proofs only
     */
    proof: () => Permission;
    /**
     * Modification is permitted by signatures only, using the private key of the zkapp account
     */
    signature: () => Permission;
    /**
     * Modification is permitted by zkapp proofs or signatures
     */
    proofOrSignature: () => Permission;
};
type Permissions_ = Update['permissions']['value'];
/**
 * Permissions specify how specific aspects of the zkapp account are allowed
 * to be modified. All fields are denominated by a {@link Permission}.
 */
interface Permissions extends Permissions_ {
    /**
     * The {@link Permission} corresponding to the 8 state fields associated with
     * an account.
     */
    editState: Permission;
    /**
     * The {@link Permission} corresponding to the ability to send transactions
     * from this account.
     */
    send: Permission;
    /**
     * The {@link Permission} corresponding to the ability to receive transactions
     * to this account.
     */
    receive: Permission;
    /**
     * The {@link Permission} corresponding to the ability to set the delegate
     * field of the account.
     */
    setDelegate: Permission;
    /**
     * The {@link Permission} corresponding to the ability to set the permissions
     * field of the account.
     */
    setPermissions: Permission;
    /**
     * The {@link Permission} corresponding to the ability to set the verification
     * key associated with the circuit tied to this account. Effectively
     * "upgradeability" of the smart contract.
     */
    setVerificationKey: Permission;
    /**
     * The {@link Permission} corresponding to the ability to set the zkapp uri
     * typically pointing to the source code of the smart contract. Usually this
     * should be changed whenever the {@link Permissions.setVerificationKey} is
     * changed. Effectively "upgradeability" of the smart contract.
     */
    setZkappUri: Permission;
    /**
     * The {@link Permission} corresponding to the ability to emit actions to the account.
     */
    editActionState: Permission;
    /**
     * The {@link Permission} corresponding to the ability to set the token symbol
     * for this account.
     */
    setTokenSymbol: Permission;
    incrementNonce: Permission;
    setVotingFor: Permission;
    setTiming: Permission;
    /**
     * Permission to control the ability to include _any_ account update for this
     * account in a transaction. Note that this is more restrictive than all other
     * permissions combined. For normal accounts it can safely be set to `none`,
     * but for token contracts this has to be more restrictive, to prevent
     * unauthorized token interactions -- for example, it could be
     * `proofOrSignature`.
     */
    access: Permission;
}
declare let Permissions: {
    /**
     * Default permissions are:
     *
     *   {@link Permissions.editState} = {@link Permission.proof}
     *
     *   {@link Permissions.send} = {@link Permission.signature}
     *
     *   {@link Permissions.receive} = {@link Permission.none}
     *
     *   {@link Permissions.setDelegate} = {@link Permission.signature}
     *
     *   {@link Permissions.setPermissions} = {@link Permission.signature}
     *
     *   {@link Permissions.setVerificationKey} = {@link Permission.signature}
     *
     *   {@link Permissions.setZkappUri} = {@link Permission.signature}
     *
     *   {@link Permissions.editActionState} = {@link Permission.proof}
     *
     *   {@link Permissions.setTokenSymbol} = {@link Permission.signature}
     *
     */
    default: () => Permissions;
    initial: () => Permissions;
    dummy: () => Permissions;
    allImpossible: () => Permissions;
    fromString: (permission: AuthRequired) => Permission;
    fromJSON: (permissions: NonNullable<Types.Json.AccountUpdate['body']['update']['permissions']>) => Permissions;
    /**
     * Modification is impossible.
     */
    impossible: () => Permission;
    /**
     * Modification is always permitted
     */
    none: () => Permission;
    /**
     * Modification is permitted by zkapp proofs only
     */
    proof: () => Permission;
    /**
     * Modification is permitted by signatures only, using the private key of the zkapp account
     */
    signature: () => Permission;
    /**
     * Modification is permitted by zkapp proofs or signatures
     */
    proofOrSignature: () => Permission;
};
/**
 * The body of describing how some [[ AccountUpdate ]] should change.
 */
interface Body extends AccountUpdateBody {
    /**
     * The address for this body.
     */
    publicKey: PublicKey;
    /**
     * Specify {@link Update}s to tweakable pieces of the account record backing
     * this address in the ledger.
     */
    update: Update;
    /**
     * The TokenId for this account.
     */
    tokenId: Field;
    /**
     * By what {@link Int64} should the balance of this account change. All
     * balanceChanges must balance by the end of smart contract execution.
     */
    balanceChange: {
        magnitude: UInt64;
        sgn: Sign;
    };
    /**
     * Recent events that have been emitted from this account.
     * Events can be collected by archive nodes.
     *
     * [Check out our documentation about
     * Events!](https://docs.minaprotocol.com/zkapps/advanced-snarkyjs/events)
     */
    events: Events;
    /**
     * Recent {@link Action}s emitted from this account.
     * Actions can be collected by archive nodes and used in combination with
     * a {@link Reducer}.
     *
     * [Check out our documentation about
     * Actions!](https://docs.minaprotocol.com/zkapps/advanced-snarkyjs/actions-and-reducer)
     */
    actions: Events;
    /**
     * The type of call.
     */
    mayUseToken: MayUseToken;
    callData: Field;
    callDepth: number;
    /**
     * A list of {@link Preconditions} that need to be fulfilled in order for
     * the {@link AccountUpdate} to be valid.
     */
    preconditions: Preconditions;
    /**
     * Defines if a full commitment is required for this transaction.
     */
    useFullCommitment: Bool;
    /**
     * Defines if the fee for creating this account should be paid out of this
     * account's balance change.
     *
     * This must only be true if the balance change is larger than the account
     * creation fee and the token ID is the default.
     */
    implicitAccountCreationFee: Bool;
    /**
     * Defines if the nonce should be incremented with this {@link AccountUpdate}.
     */
    incrementNonce: Bool;
    /**
     * Defines the type of authorization that is needed for this {@link
     * AccountUpdate}.
     *
     * A authorization can be one of three types: None, Proof or Signature
     */
    authorizationKind: AccountUpdateBody['authorizationKind'];
}
declare const Body: {
    /**
     * A body that doesn't change the underlying account record
     */
    keepAll(publicKey: PublicKey, tokenId?: Field, mayUseToken?: MayUseToken): Body;
    dummy(): Body;
};
type FeePayer = Types.ZkappCommand['feePayer'];
type FeePayerUnsigned = FeePayer & {
    lazyAuthorization?: LazySignature | undefined;
};
/**
 * Either check a value or ignore it.
 *
 * Used within [[ AccountPredicate ]]s and [[ ProtocolStatePredicate ]]s.
 */
type OrIgnore<T> = {
    isSome: Bool;
    value: T;
};
/**
 * An interval representing all the values between `lower` and `upper` inclusive
 * of both the `lower` and `upper` values.
 *
 * @typeParam A something with an ordering where one can quantify a lower and
 *            upper bound.
 */
type ClosedInterval<T> = {
    lower: T;
    upper: T;
};
declare const Preconditions: {
    ignoreAll(): Preconditions;
};
type Control = Types.AccountUpdate['authorization'];
type LazyNone = {
    kind: 'lazy-none';
};
type LazySignature = {
    kind: 'lazy-signature';
    privateKey?: PrivateKey;
};
type LazyProof = {
    kind: 'lazy-proof';
    methodName: string;
    args: any[];
    previousProofs: Pickles.Proof[];
    ZkappClass: typeof SmartContract;
    memoized: {
        fields: Field[];
        aux: any[];
    }[];
    blindingValue: Field;
};
declare const TokenId: {
    default: import("./field.js").Field;
    derive(tokenOwner: PublicKey, parentTokenId?: import("./field.js").Field): Field;
    toBase58(t: import("./field.js").Field): string;
    fromBase58(base58: string): import("./field.js").Field;
    emptyValue(): import("./field.js").Field;
    toJSON(x: import("./field.js").Field): string;
    fromJSON(x: string): import("./field.js").Field;
    toFields: (x: import("./field.js").Field) => import("./field.js").Field[];
    toAuxiliary: (x?: import("./field.js").Field | undefined) => any[];
    fromFields: (x: import("./field.js").Field[], aux: any[]) => import("./field.js").Field;
    sizeInFields(): number;
    check: (x: import("./field.js").Field) => void;
    toInput: (x: import("./field.js").Field) => {
        fields?: import("./field.js").Field[] | undefined;
        packed?: [import("./field.js").Field, number][] | undefined;
    };
};
/**
 * @deprecated use `TokenId` instead of `Token.Id` and `TokenId.derive()` instead of `Token.getId()`
 */
declare class Token {
    static Id: {
        default: import("./field.js").Field;
        derive(tokenOwner: Types.PublicKey, parentTokenId?: import("./field.js").Field): import("./field.js").Field;
        toBase58(t: import("./field.js").Field): string;
        fromBase58(base58: string): import("./field.js").Field;
        emptyValue(): import("./field.js").Field;
        toJSON(x: import("./field.js").Field): string;
        fromJSON(x: string): import("./field.js").Field;
        toFields: (x: import("./field.js").Field) => import("./field.js").Field[];
        toAuxiliary: (x?: import("./field.js").Field | undefined) => any[];
        fromFields: (x: import("./field.js").Field[], aux: any[]) => import("./field.js").Field;
        sizeInFields(): number;
        check: (x: import("./field.js").Field) => void;
        toInput: (x: import("./field.js").Field) => {
            fields?: import("./field.js").Field[] | undefined;
            packed?: [import("./field.js").Field, number][] | undefined;
        };
    };
    static getId(tokenOwner: PublicKey, parentTokenId?: import("./field.js").Field): import("./field.js").Field;
    readonly id: Field;
    readonly parentTokenId: Field;
    readonly tokenOwner: PublicKey;
    constructor({ tokenOwner, parentTokenId, }: {
        tokenOwner: PublicKey;
        parentTokenId?: Field;
    });
}
/**
 * An {@link AccountUpdate} is a set of instructions for the Mina network.
 * It includes {@link Preconditions} and a list of state updates, which need to
 * be authorized by either a {@link Signature} or {@link Proof}.
 */
declare class AccountUpdate implements Types.AccountUpdate {
    id: number;
    /**
     * A human-readable label for the account update, indicating how that update
     * was created. Can be modified by applications to add richer information.
     */
    label: string;
    body: Body;
    authorization: Control;
    lazyAuthorization: LazySignature | LazyProof | LazyNone | undefined;
    account: Precondition.Account;
    network: Precondition.Network;
    currentSlot: Precondition.CurrentSlot;
    children: {
        callsType: {
            type: 'None';
        } | {
            type: 'Witness';
        } | {
            type: 'Equals';
            value: Field;
        };
        accountUpdates: AccountUpdate[];
    };
    parent: AccountUpdate | undefined;
    private isSelf;
    static Actions: {
        toFields: (x: {
            data: import("./field.js").Field[][];
            hash: import("./field.js").Field;
        }) => import("./field.js").Field[];
        toAuxiliary: (x?: {
            data: import("./field.js").Field[][];
            hash: import("./field.js").Field;
        } | undefined) => any[];
        fromFields: (x: import("./field.js").Field[], aux: any[]) => {
            data: import("./field.js").Field[][];
            hash: import("./field.js").Field;
        };
        sizeInFields(): number;
        check: (x: {
            data: import("./field.js").Field[][];
            hash: import("./field.js").Field;
        }) => void;
        toInput: (x: {
            data: import("./field.js").Field[][];
            hash: import("./field.js").Field;
        }) => {
            fields?: import("./field.js").Field[] | undefined;
            packed?: [import("./field.js").Field, number][] | undefined;
        };
        toJSON: (x: {
            data: import("./field.js").Field[][];
            hash: import("./field.js").Field;
        }) => string[][];
        fromJSON: (x: string[][]) => {
            data: import("./field.js").Field[][];
            hash: import("./field.js").Field;
        };
        emptyValue: (() => {
            data: import("./field.js").Field[][];
            hash: import("./field.js").Field;
        }) & (() => {
            data: import("./field.js").Field[][];
            hash: import("./field.js").Field;
        });
        empty(): {
            hash: import("./field.js").Field;
            data: import("./field.js").Field[][];
        };
        pushEvent(actions: {
            hash: import("./field.js").Field;
            data: import("./field.js").Field[][];
        }, event: import("./field.js").Field[]): {
            hash: import("./field.js").Field;
            data: import("./field.js").Field[][];
        };
        fromList(events: import("./field.js").Field[][]): {
            hash: import("./field.js").Field;
            data: import("./field.js").Field[][];
        };
        hash(events: import("./field.js").Field[][]): import("./field.js").Field;
        emptyActionState(): import("./field.js").Field;
        updateSequenceState(state: import("./field.js").Field, sequenceEventsHash: import("./field.js").Field): import("./field.js").Field;
    };
    constructor(body: Body, authorization?: Control);
    /**
     * Clones the {@link AccountUpdate}.
     */
    static clone(accountUpdate: AccountUpdate): AccountUpdate;
    token(): {
        id: import("./field.js").Field;
        parentTokenId: import("./field.js").Field;
        tokenOwner: Types.PublicKey;
        /**
         * Mints token balance to `address`. Returns the mint account update.
         */
        mint({ address, amount, }: {
            address: PublicKey | AccountUpdate | SmartContract;
            amount: number | bigint | UInt64;
        }): AccountUpdate;
        /**
         * Burn token balance on `address`. Returns the burn account update.
         */
        burn({ address, amount, }: {
            address: PublicKey | AccountUpdate | SmartContract;
            amount: number | bigint | UInt64;
        }): AccountUpdate;
        /**
         * Move token balance from `from` to `to`. Returns the `to` account update.
         */
        send({ from, to, amount, }: {
            from: PublicKey | AccountUpdate | SmartContract;
            to: PublicKey | AccountUpdate | SmartContract;
            amount: number | bigint | UInt64;
        }): AccountUpdate;
    };
    get tokenId(): import("./field.js").Field;
    /**
     * @deprecated use `this.account.tokenSymbol`
     */
    get tokenSymbol(): {
        set(tokenSymbol: string): void;
    };
    send({ to, amount, }: {
        to: PublicKey | AccountUpdate | SmartContract;
        amount: number | bigint | UInt64;
    }): AccountUpdate;
    /**
     * Makes an {@link AccountUpdate} a child-{@link AccountUpdate} of this and
     * approves it.
     */
    approve(childUpdate: AccountUpdate, layout?: AccountUpdatesLayout): void;
    get balance(): {
        addInPlace(x: Int64 | UInt32 | UInt64 | string | number | bigint): void;
        subInPlace(x: Int64 | UInt32 | UInt64 | string | number | bigint): void;
    };
    get update(): Update;
    static setValue<T>(maybeValue: SetOrKeep<T>, value: T): void;
    /**
     * Constrain a property to lie between lower and upper bounds.
     *
     * @param property The property to constrain
     * @param lower The lower bound
     * @param upper The upper bound
     *
     * Example: To constrain the account balance of a SmartContract to lie between
     * 0 and 20 MINA, you can use
     *
     * ```ts
     * \@method onlyRunsWhenBalanceIsLow() {
     *   let lower = UInt64.zero;
     *   let upper = UInt64.from(20e9);
     *   AccountUpdate.assertBetween(this.self.body.preconditions.account.balance, lower, upper);
     *   // ...
     * }
     * ```
     */
    static assertBetween<T>(property: OrIgnore<ClosedInterval<T>>, lower: T, upper: T): void;
    /**
     * Fix a property to a certain value.
     *
     * @param property The property to constrain
     * @param value The value it is fixed to
     *
     * Example: To fix the account nonce of a SmartContract to 0, you can use
     *
     * ```ts
     * \@method onlyRunsWhenNonceIsZero() {
     *   AccountUpdate.assertEquals(this.self.body.preconditions.account.nonce, UInt32.zero);
     *   // ...
     * }
     * ```
     */
    static assertEquals<T extends object>(property: OrIgnore<ClosedInterval<T> | T>, value: T): void;
    get publicKey(): PublicKey;
    /**
     * Use this command if this account update should be signed by the account
     * owner, instead of not having any authorization.
     *
     * If you use this and are not relying on a wallet to sign your transaction,
     * then you should use the following code before sending your transaction:
     *
     * ```ts
     * let tx = Mina.transaction(...); // create transaction as usual, using `requireSignature()` somewhere
     * tx.sign([privateKey]); // pass the private key of this account to `sign()`!
     * ```
     *
     * Note that an account's {@link Permissions} determine which updates have to
     * be (can be) authorized by a signature.
     */
    requireSignature(): void;
    /**
     * @deprecated `.sign()` is deprecated in favor of `.requireSignature()`
     */
    sign(privateKey?: PrivateKey): void;
    static signFeePayerInPlace(feePayer: FeePayerUnsigned, privateKey?: PrivateKey): void;
    static getNonce(accountUpdate: AccountUpdate | FeePayerUnsigned): Types.UInt32;
    private static signingInfo;
    private static getSigningInfo;
    private static getSigningInfoUnchecked;
    toJSON(): Types.Json.AccountUpdate;
    static toJSON(a: AccountUpdate): Types.Json.AccountUpdate;
    static fromJSON(json: Types.Json.AccountUpdate): AccountUpdate;
    hash(): Field;
    toPublicInput(): ZkappPublicInput;
    static defaultAccountUpdate(address: PublicKey, tokenId?: Field): AccountUpdate;
    static dummy(): AccountUpdate;
    isDummy(): import("./bool.js").Bool;
    static defaultFeePayer(address: PublicKey, nonce: UInt32): FeePayerUnsigned;
    static dummyFeePayer(): FeePayerUnsigned;
    /**
     * Creates an account update. If this is inside a transaction, the account
     * update becomes part of the transaction. If this is inside a smart contract
     * method, the account update will not only become part of the transaction,
     * but also becomes available for the smart contract to modify, in a way that
     * becomes part of the proof.
     */
    static create(publicKey: PublicKey, tokenId?: Field): AccountUpdate;
    /**
     * Attach account update to the current transaction
     * -- if in a smart contract, to its children
     */
    static attachToTransaction(accountUpdate: AccountUpdate): void;
    /**
     * Disattach an account update from where it's currently located in the transaction
     */
    static unlink(accountUpdate: AccountUpdate): void;
    /**
     * Creates an account update, like {@link AccountUpdate.create}, but also
     * makes sure this account update will be authorized with a signature.
     *
     * If you use this and are not relying on a wallet to sign your transaction,
     * then you should use the following code before sending your transaction:
     *
     * ```ts
     * let tx = Mina.transaction(...); // create transaction as usual, using `createSigned()` somewhere
     * tx.sign([privateKey]); // pass the private key of this account to `sign()`!
     * ```
     *
     * Note that an account's {@link Permissions} determine which updates have to
     * be (can be) authorized by a signature.
     */
    static createSigned(signer: PublicKey, tokenId?: Field): AccountUpdate;
    /**
     * @deprecated in favor of calling this function with a `PublicKey` as `signer`
     */
    static createSigned(signer: PrivateKey, tokenId?: Field): AccountUpdate;
    /**
     * Use this method to pay the account creation fee for another account (or, multiple accounts using the optional second argument).
     *
     * Beware that you _don't_ need to specify the account that is created!
     * Instead, the protocol will automatically identify that accounts need to be created,
     * and require that the net balance change of the transaction covers the account creation fee.
     *
     * @param feePayer the address of the account that pays the fee
     * @param numberOfAccounts the number of new accounts to fund (default: 1)
     * @returns they {@link AccountUpdate} for the account which pays the fee
     */
    static fundNewAccount(feePayer: PublicKey, numberOfAccounts?: number): AccountUpdate;
    /**
     * @deprecated Call this function with a `PublicKey` as `feePayer`, and remove the `initialBalance` option.
     * To send an initial balance to the new account, you can use the returned account update:
     * ```
     * let feePayerUpdate = AccountUpdate.fundNewAccount(feePayer);
     * feePayerUpdate.send({ to: receiverAddress, amount: initialBalance });
     * ```
     */
    static fundNewAccount(feePayer: PrivateKey | PublicKey, options?: {
        initialBalance: number | string | UInt64;
    } | number): AccountUpdate;
    static sizeInFields: () => number;
    static toFields: (value: Types.AccountUpdate) => import("./field.js").Field[];
    static toAuxiliary(a?: AccountUpdate): (any[] | {
        lazyAuthorization: LazySignature | LazyProof | LazyNone | undefined;
        children: {
            callsType: {
                type: "None";
            } | {
                type: "Witness";
            } | {
                type: "Equals";
                value: import("./field.js").Field;
            };
            accountUpdates: AccountUpdate[];
        };
        parent: AccountUpdate | undefined;
        id: number;
        label: string;
    })[];
    static toInput: (value: Types.AccountUpdate) => {
        fields?: import("./field.js").Field[] | undefined;
        packed?: [import("./field.js").Field, number][] | undefined;
    };
    static check: (value: Types.AccountUpdate) => void;
    static fromFields(fields: Field[], [other, aux]: any[]): AccountUpdate;
    static witness<T>(type: FlexibleProvable<T>, compute: () => {
        accountUpdate: AccountUpdate;
        result: T;
    }, { skipCheck }?: {
        skipCheck?: boolean | undefined;
    }): {
        accountUpdate: AccountUpdate;
        result: T;
    };
    static witnessChildren(accountUpdate: AccountUpdate, childLayout: AccountUpdatesLayout, options?: {
        skipCheck: boolean;
    }): void;
    /**
     * Like AccountUpdate.witness, but lets you specify a layout for the
     * accountUpdate's children, which also get witnessed
     */
    static witnessTree<T>(resultType: FlexibleProvable<T>, childLayout: AccountUpdatesLayout, compute: () => {
        accountUpdate: AccountUpdate;
        result: T;
    }, options?: {
        skipCheck: boolean;
    }): {
        accountUpdate: AccountUpdate;
        result: T;
    };
    /**
     * Describes the children of an account update, which are laid out in a tree.
     *
     * The tree layout is described recursively by using a combination of `AccountUpdate.Layout.NoChildren`, `AccountUpdate.Layout.StaticChildren(...)` and `AccountUpdate.Layout.AnyChildren`.
     * - `NoChildren` means an account update that can't have children
     * - `AnyChildren` means an account update can have an arbitrary amount of children, which means you can't access those children in your circuit (because the circuit is static).
     * - `StaticChildren` means the account update must have a certain static amount of children and expects as arguments a description of each of those children.
     *   As a shortcut, you can also pass `StaticChildren` a number, which means it has that amount of children but no grandchildren.
     *
     * This is best understood by examples:
     *
     * ```ts
     * let { NoChildren, AnyChildren, StaticChildren } = AccounUpdate.Layout;
     *
     * NoChildren                 // an account update with no children
     * AnyChildren                // an account update with arbitrary children
     * StaticChildren(NoChildren) // an account update with 1 child, which doesn't have children itself
     * StaticChildren(1)          // shortcut for StaticChildren(NoChildren)
     * StaticChildren(2)          // shortcut for StaticChildren(NoChildren, NoChildren)
     * StaticChildren(0)          // equivalent to NoChildren
     *
     * // an update with 2 children, of which one has arbitrary children and the other has exactly 1 descendant
     * StaticChildren(AnyChildren, StaticChildren(1))
     * ```
     */
    static Layout: {
        StaticChildren: {
            (n: number): AccountUpdatesLayout;
            (...args: AccountUpdatesLayout[]): AccountUpdatesLayout;
        };
        NoChildren: number;
        AnyChildren: "AnyChildren";
        NoDelegation: "NoDelegation";
    };
    static get MayUseToken(): {
        type: import("../snarky.js").ProvablePure<{
            parentsOwnToken: import("./bool.js").Bool;
            inheritFromParent: import("./bool.js").Bool;
        }> & {
            toInput: (x: {
                parentsOwnToken: import("./bool.js").Bool;
                inheritFromParent: import("./bool.js").Bool;
            }) => {
                fields?: import("./field.js").Field[] | undefined;
                packed?: [import("./field.js").Field, number][] | undefined;
            };
            toJSON: (x: {
                parentsOwnToken: import("./bool.js").Bool;
                inheritFromParent: import("./bool.js").Bool;
            }) => {
                parentsOwnToken: boolean;
                inheritFromParent: boolean;
            };
            fromJSON: (x: {
                parentsOwnToken: boolean;
                inheritFromParent: boolean;
            }) => {
                parentsOwnToken: import("./bool.js").Bool;
                inheritFromParent: import("./bool.js").Bool;
            };
        };
        No: {
            parentsOwnToken: import("./bool.js").Bool;
            inheritFromParent: import("./bool.js").Bool;
        };
        ParentsOwnToken: {
            parentsOwnToken: import("./bool.js").Bool;
            inheritFromParent: import("./bool.js").Bool;
        };
        InheritFromParent: {
            parentsOwnToken: import("./bool.js").Bool;
            inheritFromParent: import("./bool.js").Bool;
        };
        isNo({ body: { mayUseToken: { parentsOwnToken, inheritFromParent }, }, }: AccountUpdate): import("./bool.js").Bool;
        isParentsOwnToken(a: AccountUpdate): import("./bool.js").Bool;
        isInheritFromParent(a: AccountUpdate): import("./bool.js").Bool;
    };
    /**
     * Returns a JSON representation of only the fields that differ from the
     * default {@link AccountUpdate}.
     */
    toPretty(): any;
}
type AccountUpdatesLayout = number | 'AnyChildren' | 'NoDelegation' | AccountUpdatesLayout[];
type WithCallers = {
    accountUpdate: AccountUpdate;
    caller: Field;
    children: WithCallers[];
};
declare const CallForest: {
    toFlatList(forest: AccountUpdate[], mutate?: boolean, depth?: number): AccountUpdate[];
    emptyHash(): import("./field.js").Field;
    hashChildren(update: AccountUpdate): Field;
    hashChildrenBase({ children }: AccountUpdate): import("./field.js").Field;
    addCallers(updates: AccountUpdate[], context?: {
        self: Field;
        caller: Field;
    }): WithCallers[];
    /**
     * Used in the prover to witness the context from which to compute its caller
     *
     * TODO: currently unused, but could come back when we add caller to the
     * public input
     */
    computeCallerContext(update: AccountUpdate): {
        self: import("./field.js").Field;
        caller: import("./field.js").Field;
    };
    callerContextType: import("../snarky.js").ProvablePure<{
        self: import("./field.js").Field;
        caller: import("./field.js").Field;
    }> & {
        toInput: (x: {
            self: import("./field.js").Field;
            caller: import("./field.js").Field;
        }) => {
            fields?: import("./field.js").Field[] | undefined;
            packed?: [import("./field.js").Field, number][] | undefined;
        };
        toJSON: (x: {
            self: import("./field.js").Field;
            caller: import("./field.js").Field;
        }) => {
            self: string;
            caller: string;
        };
        fromJSON: (x: {
            self: string;
            caller: string;
        }) => {
            self: import("./field.js").Field;
            caller: import("./field.js").Field;
        };
    };
    computeCallDepth(update: AccountUpdate): number;
    map(updates: AccountUpdate[], map: (update: AccountUpdate) => AccountUpdate): AccountUpdate[];
    forEach(updates: AccountUpdate[], callback: (update: AccountUpdate) => void): void;
    forEachPredecessor(updates: AccountUpdate[], update: AccountUpdate, callback: (update: AccountUpdate) => void): void;
};
declare function createChildAccountUpdate(parent: AccountUpdate, childAddress: PublicKey, tokenId?: Field): AccountUpdate;
type ZkappCommand = {
    feePayer: FeePayerUnsigned;
    accountUpdates: AccountUpdate[];
    memo: string;
};
type ZkappCommandSigned = {
    feePayer: FeePayer;
    accountUpdates: (AccountUpdate & {
        lazyAuthorization?: LazyProof;
    })[];
    memo: string;
};
type ZkappCommandProved = {
    feePayer: FeePayerUnsigned;
    accountUpdates: (AccountUpdate & {
        lazyAuthorization?: LazySignature;
    })[];
    memo: string;
};
declare const ZkappCommand: {
    toPretty(transaction: ZkappCommand): any[];
    fromJSON(json: Types.Json.ZkappCommand): ZkappCommand;
    toJSON({ feePayer, accountUpdates, memo }: ZkappCommand): Types.Json.ZkappCommand;
};
type AccountUpdateProved = AccountUpdate & {
    lazyAuthorization?: LazySignature;
};
declare const Authorization: {
    hasLazyProof(accountUpdate: AccountUpdate): boolean;
    hasAny(accountUpdate: AccountUpdate): boolean;
    setSignature(accountUpdate: AccountUpdate, signature: string): void;
    setProof(accountUpdate: AccountUpdate, proof: string): AccountUpdateProved;
    setLazySignature(accountUpdate: AccountUpdate, signature?: Omit<LazySignature, 'kind'>): void;
    setProofAuthorizationKind({ body, id }: AccountUpdate, priorAccountUpdates?: AccountUpdate[]): void;
    setLazyProof(accountUpdate: AccountUpdate, proof: Omit<LazyProof, 'kind'>, priorAccountUpdates: AccountUpdate[]): void;
    setLazyNone(accountUpdate: AccountUpdate): void;
};
declare function addMissingSignatures(zkappCommand: ZkappCommand, additionalKeys?: PrivateKey[]): ZkappCommandSigned;
declare function dummySignature(): string;
/**
 * The public input for zkApps consists of certain hashes of the proving
 AccountUpdate (and its child accountUpdates) which is constructed during method
 execution.

  For SmartContract proving, a method is run twice: First outside the proof, to
 obtain the public input, and once in the prover, which takes the public input
 as input. The current transaction is hashed again inside the prover, which
 asserts that the result equals the input public input, as part of the snark
 circuit. The block producer will also hash the transaction they receive and
 pass it as a public input to the verifier. Thus, the transaction is fully
 constrained by the proof - the proof couldn't be used to attest to a different
 transaction.
 */
type ZkappPublicInput = {
    accountUpdate: Field;
    calls: Field;
};
declare let ZkappPublicInput: import("../snarky.js").ProvablePure<{
    accountUpdate: import("./field.js").Field;
    calls: import("./field.js").Field;
}> & {
    toInput: (x: {
        accountUpdate: import("./field.js").Field;
        calls: import("./field.js").Field;
    }) => {
        fields?: import("./field.js").Field[] | undefined;
        packed?: [import("./field.js").Field, number][] | undefined;
    };
    toJSON: (x: {
        accountUpdate: import("./field.js").Field;
        calls: import("./field.js").Field;
    }) => {
        accountUpdate: string;
        calls: string;
    };
    fromJSON: (x: {
        accountUpdate: string;
        calls: string;
    }) => {
        accountUpdate: import("./field.js").Field;
        calls: import("./field.js").Field;
    };
};
declare function addMissingProofs(zkappCommand: ZkappCommand, { proofsEnabled }: {
    proofsEnabled?: boolean | undefined;
}): Promise<{
    zkappCommand: ZkappCommandProved;
    proofs: (Proof<ZkappPublicInput, Empty> | undefined)[];
}>;
