import { Types } from '../../bindings/mina-transaction/types.js';
import { Bool, Field } from '../core.js';
import { Permissions } from '../account_update.js';
import { UInt32, UInt64 } from '../int.js';
import { PublicKey } from '../signature.js';
import { TokenId, ReceiptChainHash } from '../base58-encodings.js';
import { genericLayoutFold } from '../../bindings/lib/from-layout.js';
import { customTypes, TypeMap, } from '../../bindings/mina-transaction/gen/transaction.js';
import { jsLayout } from '../../bindings/mina-transaction/gen/js-layout.js';
export { Account };
export { accountQuery, parseFetchedAccount, fillPartialAccount };
const Account = Types.Account;
const accountQuery = (publicKey, tokenId) => `{
  account(publicKey: "${publicKey}", token: "${tokenId}") {
    publicKey
    token
    nonce
    balance { total }
    tokenSymbol
    receiptChainHash
    timing {
      initialMinimumBalance
      cliffTime
      cliffAmount
      vestingPeriod
      vestingIncrement
    }
    permissions {
      editState
      access
      send
      receive
      setDelegate
      setPermissions
      setVerificationKey
      setZkappUri
      editActionState
      setTokenSymbol
      incrementNonce
      setVotingFor
      setTiming
    }
    delegateAccount { publicKey }
    votingFor
    zkappState
    verificationKey {
      verificationKey
      hash
    }
    actionState
    provedState
    zkappUri
  }
}
`;
// convert FetchedAccount (from graphql) to Account (internal representation both here and in Mina)
function parseFetchedAccount({ publicKey, nonce, zkappState, balance, permissions, timing: { cliffAmount, cliffTime, initialMinimumBalance, vestingIncrement, vestingPeriod, }, delegateAccount, receiptChainHash, actionState, token, tokenSymbol, verificationKey, provedState, zkappUri, }) {
    let hasZkapp = zkappState !== null ||
        verificationKey !== null ||
        actionState !== null ||
        zkappUri !== null ||
        provedState;
    let partialAccount = {
        publicKey: PublicKey.fromBase58(publicKey),
        tokenId: TokenId.fromBase58(token),
        tokenSymbol: tokenSymbol ?? undefined,
        balance: balance && UInt64.from(balance.total),
        nonce: UInt32.from(nonce),
        receiptChainHash: (receiptChainHash && ReceiptChainHash.fromBase58(receiptChainHash)) ||
            undefined,
        delegate: (delegateAccount && PublicKey.fromBase58(delegateAccount.publicKey)) ??
            undefined,
        votingFor: undefined,
        timing: (cliffAmount &&
            cliffTime &&
            initialMinimumBalance &&
            vestingIncrement &&
            vestingPeriod && {
            isTimed: Bool(true),
            cliffAmount: UInt64.from(cliffAmount),
            cliffTime: UInt32.from(cliffTime),
            initialMinimumBalance: UInt64.from(initialMinimumBalance),
            vestingIncrement: UInt64.from(vestingIncrement),
            vestingPeriod: UInt32.from(vestingPeriod),
        }) ||
            undefined,
        permissions: (permissions && Permissions.fromJSON(permissions)) ??
            Permissions.initial(),
        zkapp: hasZkapp
            ? {
                appState: (zkappState && zkappState.map(Field)) ?? undefined,
                verificationKey: (verificationKey && {
                    data: verificationKey.verificationKey,
                    hash: Field(verificationKey.hash),
                }) ??
                    undefined,
                zkappVersion: undefined,
                actionState: (actionState && actionState.map(Field)) ?? undefined,
                lastActionSlot: undefined,
                provedState: provedState !== null ? Bool(provedState) : undefined,
                zkappUri: zkappUri !== null ? zkappUri : undefined,
            }
            : undefined,
    };
    return fillPartialAccount(partialAccount);
}
function fillPartialAccount(account) {
    return genericLayoutFold(TypeMap, customTypes, {
        map(type, value) {
            // if value exists, use it; otherwise fall back to dummy value
            if (value !== undefined)
                return value;
            // fall back to dummy value
            if (type.emptyValue)
                return type.emptyValue();
            return type.fromFields(Array(type.sizeInFields()).fill(Field(0)), type.toAuxiliary());
        },
        reduceArray(array) {
            return array;
        },
        reduceObject(_, record) {
            return record;
        },
        reduceFlaggedOption() {
            // doesn't occur for accounts
            throw Error('not relevant');
        },
        reduceOrUndefined(value) {
            // don't fill in value that's allowed to be undefined
            return value;
        },
    }, jsLayout.Account, account);
}
//# sourceMappingURL=account.js.map