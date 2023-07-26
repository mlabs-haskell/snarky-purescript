export { prefixes, versionBytes, poseidonParamsKimchiFp, poseidonParamsLegacyFp, mocks };
declare let prefixes: {
    event: string;
    events: string;
    sequenceEvents: string;
    body: string;
    accountUpdateCons: string;
    accountUpdateNode: string;
    zkappMemo: string;
    signatureMainnet: string;
    signatureTestnet: string;
    zkappUri: string;
    deriveTokenId: string;
};
declare let versionBytes: {
    tokenIdKey: number;
    receiptChainHash: number;
    ledgerHash: number;
    epochSeed: number;
    stateHash: number;
    publicKey: number;
    userCommandMemo: number;
    privateKey: number;
    signature: number;
    transactionHash: number;
    signedCommandV1: number;
};
declare let poseidonParamsKimchiFp: {
    mds: string[][];
    roundConstants: string[][];
    fullRounds: number;
    partialRounds: number;
    hasInitialRoundConstant: boolean;
    stateSize: number;
    rate: number;
    power: number;
};
declare let poseidonParamsLegacyFp: {
    mds: string[][];
    roundConstants: string[][];
    fullRounds: number;
    partialRounds: number;
    hasInitialRoundConstant: boolean;
    stateSize: number;
    rate: number;
    power: number;
};
declare let mocks: {
    dummyVerificationKeyHash: string;
};
