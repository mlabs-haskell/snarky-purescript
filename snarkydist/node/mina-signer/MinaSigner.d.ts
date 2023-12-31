import * as Json from './src/TSTypes.js';
import type { SignedLegacy, Signed, Network } from './src/TSTypes.js';
import * as TransactionJson from '../bindings/mina-transaction/gen/transaction-json.js';
export { Client as default };
declare class Client {
    private network;
    constructor(options: {
        network: Network;
    });
    /**
     * Generates a public/private key pair
     *
     * @returns A Mina key pair
     */
    genKeys(): Json.Keypair;
    /**
     * Verifies if a key pair is valid by checking if the public key can be derived from
     * the private key and additionally checking if we can use the private key to
     * sign a transaction. If the key pair is invalid, an exception is thrown.
     *
     * @param keypair A key pair
     * @returns True if the `keypair` is a verifiable key pair, otherwise throw an exception
     */
    verifyKeypair({ privateKey, publicKey }: Json.Keypair): boolean;
    /**
     * Derives the public key of the corresponding private key
     *
     * @param privateKey The private key used to get the corresponding public key
     * @returns A public key
     */
    derivePublicKey(privateKeyBase58: Json.PrivateKey): Json.PublicKey;
    /**
     * Signs an arbitrary list of field elements in a SNARK-compatible way.
     * The resulting signature can be verified in SnarkyJS as follows:
     * ```ts
     * // sign field elements with mina-signer
     * let signed = client.signFields(fields, privateKey);
     *
     * // read signature in snarkyjs and verify
     * let signature = Signature.fromBase58(signed.signature);
     * let isValid: Bool = signature.verify(publicKey, fields.map(Field));
     * ```
     *
     * @param fields An arbitrary list of field elements
     * @param privateKey The private key used for signing
     * @returns The signed field elements
     */
    signFields(fields: bigint[], privateKey: Json.PrivateKey): Signed<bigint[]>;
    /**
     * Verifies a signature created by {@link signFields}.
     *
     * @param signedFields The signed field elements
     * @returns True if the `signedFields` contains a valid signature matching
     * the fields and publicKey.
     */
    verifyFields({ data, signature, publicKey }: Signed<bigint[]>): boolean;
    /**
     * Signs an arbitrary message
     *
     * @param message An arbitrary string message to be signed
     * @param privateKey The private key used to sign the message
     * @returns A signed message
     */
    signMessage(message: string, privateKey: Json.PrivateKey): SignedLegacy<string>;
    /**
     * Verifies a signature created by {@link signMessage}.
     *
     * @param signedMessage A signed message
     * @returns True if the `signedMessage` contains a valid signature matching
     * the message and publicKey.
     */
    verifyMessage({ data, signature, publicKey }: SignedLegacy<string>): boolean;
    /**
     * Signs a payment transaction using a private key.
     *
     * This type of transaction allows a user to transfer funds from one account
     * to another over the network.
     *
     * @param payment An object describing the payment
     * @param privateKey The private key used to sign the transaction
     * @returns A signed payment transaction
     */
    signPayment(payment: Json.Payment, privateKey: Json.PrivateKey): SignedLegacy<Json.Payment>;
    /**
     * Verifies a signed payment.
     *
     * @param signedPayment A signed payment transaction
     * @returns True if the `signedPayment` is a verifiable payment
     */
    verifyPayment({ data, signature, publicKey, }: SignedLegacy<Json.Payment>): boolean;
    /**
     * Signs a stake delegation transaction using a private key.
     *
     * This type of transaction allows a user to delegate their
     * funds from one account to another for use in staking. The
     * account that is delegated to is then considered as having these
     * funds when determining whether it can produce a block in a given slot.
     *
     * @param delegation An object describing the stake delegation
     * @param privateKey The private key used to sign the transaction
     * @returns A signed stake delegation
     */
    signStakeDelegation(delegation: Json.StakeDelegation, privateKey: Json.PrivateKey): SignedLegacy<Json.StakeDelegation>;
    /**
     * Verifies a signed stake delegation.
     *
     * @param signedStakeDelegation A signed stake delegation
     * @returns True if the `signedStakeDelegation` is a verifiable stake delegation
     */
    verifyStakeDelegation({ data, signature, publicKey, }: SignedLegacy<Json.StakeDelegation>): boolean;
    /**
     * Compute the hash of a signed payment.
     *
     * @param signedPayment A signed payment transaction
     * @returns A transaction hash
     */
    hashPayment({ data, signature }: SignedLegacy<Json.Payment>, options?: {
        berkeley?: boolean;
    }): string;
    /**
     * Compute the hash of a signed stake delegation.
     *
     * @param signedStakeDelegation A signed stake delegation
     * @returns A transaction hash
     */
    hashStakeDelegation({ data, signature }: SignedLegacy<Json.StakeDelegation>, options?: {
        berkeley?: boolean;
    }): string;
    /**
     * Sign a zkapp command transaction using a private key.
     *
     * This type of transaction allows a user to update state on a given
     * Smart Contract running on Mina.
     *
     * @param zkappCommand An object representing a zkApp transaction
     * @param privateKey The fee payer private key
     * @returns Signed `zkappCommand`
     */
    signZkappCommand({ feePayer: feePayer_, zkappCommand }: Json.ZkappCommand, privateKey: Json.PrivateKey): Signed<Json.ZkappCommand>;
    /**
     * Verifies a signed zkApp transaction.
     *
     * @param signedZkappCommand A signed zkApp transaction
     * @returns True if the signature is valid
     */
    verifyZkappCommand({ data, publicKey, signature, }: Signed<Json.ZkappCommand>): boolean;
    /**
     * Converts a Rosetta signed transaction to a JSON string that is
     * compatible with GraphQL. The JSON string is a representation of
     * a `Signed_command` which is what our GraphQL expects.
     *
     * @param signedRosettaTxn A signed Rosetta transaction
     * @returns A string that represents the JSON conversion of a signed Rosetta transaction`.
     */
    signedRosettaTransactionToSignedCommand(signedRosettaTxn: string): string;
    /**
     * Return the hex-encoded format of a valid public key. This will throw an exception if
     * the key is invalid or the conversion fails.
     *
     * @param publicKey A valid public key
     * @returns A string that represents the hex encoding of a public key.
     */
    publicKeyToRaw(publicKeyBase58: string): string;
    /**
     * Signs an arbitrary payload using a private key. This function can sign strings,
     * payments, stake delegations, and zkapp commands. If the payload is unrecognized, an Error
     * is thrown.
     *
     * @param payload A signable payload
     * @param privateKey A private key
     * @returns A signed payload
     */
    signTransaction<T extends Json.SignableData | Json.ZkappCommand>(payload: T, privateKey: Json.PrivateKey): T extends Json.SignableData ? SignedLegacy<T> : Signed<T>;
    /**
     * Verifies a signed payload. The payload can be a string, payment, stake delegation or zkApp transaction.
     * If the payload is unrecognized, an Error is thrown.
     *
     * @param signedPayload A signed payload
     * @returns True if the signature is valid
     */
    verifyTransaction(signed: SignedLegacy<Json.SignableData> | Signed<Json.ZkappCommand>): boolean;
    /**
     * Calculates the minimum fee of a zkapp command transaction. A fee for a zkapp command transaction is
     * the sum of all account updates plus the specified fee amount. If no fee is passed in, `0.001`
     * is used (according to the Mina spec) by default.
     * @param accountUpdates A list of account updates
     * @returns  The fee to be paid by the fee payer accountUpdate
     */
    getAccountUpdateMinimumFee(accountUpdates: TransactionJson.AccountUpdate[]): number;
    /**
     * Creates a nullifier
     *
     * @param message A unique message that belongs to a specific nullifier
     * @param privateKeyBase58 The private key used to create the nullifier
     * @returns A nullifier
     */
    createNullifier(message: bigint[], privateKeyBase58: Json.PrivateKey): Json.Nullifier;
}
