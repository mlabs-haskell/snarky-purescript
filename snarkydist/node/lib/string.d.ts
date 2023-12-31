import { Bool, Field } from '../lib/core.js';
import { CircuitValue } from './circuit_value.js';
export { Character, CircuitString };
declare class Character extends CircuitValue {
    value: Field;
    isNull(): Bool;
    toField(): Field;
    toString(): string;
    static fromString(str: string): Character;
    static check(c: Character): void;
}
declare class CircuitString extends CircuitValue {
    static maxLength: number;
    values: Character[];
    private constructor();
    static fromCharacters(chars: Character[]): CircuitString;
    private maxLength;
    private computeLengthAndMask;
    private lengthMask;
    private length;
    /**
     * appends another string to this one, returns the result and proves that it fits
     * within the `maxLength` of this string (the other string can have a different maxLength)
     */
    append(str: CircuitString): CircuitString;
    /**
     * returns true if `str` is found in this `CircuitString`
     */
    hash(): Field;
    substring(start: number, end: number): CircuitString;
    toString(): string;
    static fromString(str: string): CircuitString;
}
