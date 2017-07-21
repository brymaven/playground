module Playground.Types where

-- | Mapping from States -> Actions
newtype Policy s a = Policy { runPolicy :: s -> a }

--
type State = Int
type Action = Int

-- | Goal
type Reward = Double

-- Reward signal indicates what is good in an immediate sense, a value function specifies
-- what is good in the long run.

-- Value of a state is the total amount of reward an agent can expect to accumulate over the future, starting from that state.


-- Rewards are primary.
-- Values as predictions of rewards are secondary. Without rewards, there can be no values.
