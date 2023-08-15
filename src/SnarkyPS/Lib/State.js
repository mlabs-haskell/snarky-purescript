import {State} from "../../snarkydist/node/lib/state.js";

export function setState(value) {
  return function(state){
    state.set(value);
  };
};

export function getState(state) {
  return state.get();
};

export function assertEqState(value) {
  return function(state){
    state.assertEquals(value);
  };
};

export function getAndAssertEqState (state) {
  return state.getAndAssertEquals();
};

export function newState(){
  return State();
};