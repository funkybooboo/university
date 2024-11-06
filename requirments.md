Objective:

This assignment introduces you to reinforcement learning (RL) using Stable Baselines3, with a focus on modifying the reward function and experimenting with different model architectures. You will observe the impact of these changes on agent performance.
Install Stable Baselines3 and Gym

    Install the required libraries:
    pip install stable-baselines3 gym

    Verify the installation by importing the libraries:
    import gym from stable_baselines3 import PPO
    Set Up the CartPole Environment and Baseline Training

        Use the CartPole-v1 environment as before:
        import gym from stable_baselines3 import PPO env = gym.make("CartPole-v1")

        Define and train a baseline agent using PPO without any modifications:
        model = PPO("MlpPolicy", env, verbose=1) model.learn(total_timesteps=10000)

        Evaluate the agent’s baseline performance (follow the same steps as before for evaluation).

    Record the average reward as a baseline for later comparison.

     
    Modify the Reward Function

    To encourage different behavior, modify the reward function in the CartPole environment. For example, you can increase the reward for balancing longer or penalize more for tilting.

        Define a custom environment by creating a new class that wraps CartPole and modifies the reward function:

    import numpy as np
    from gym import RewardWrapper

    class CustomCartPoleReward(RewardWrapper):
        def __init__(self, env):
            super(CustomCartPoleReward, self).__init__(env)

        def reward(self, reward):
            # Increase reward for keeping the pole upright and penalize for moving away from the center
            x, x_dot, theta, theta_dot = self.env.state
            new_reward = reward - np.abs(theta)  # Penalize for angle from upright
            return new_reward

    # Initialize the custom environment
    custom_env = CustomCartPoleReward(gym.make("CartPole-v1"))

        Train the agent with the modified reward function:
        custom_model = PPO("MlpPolicy", custom_env, verbose=1)
        custom_model.learn(total_timesteps=10000)

        Evaluate the agent with the modified reward function:

        custom_rewards = []
        episodes = 10

        for episode in range(episodes):
            state = custom_env.reset()
            done = False
            episode_reward = 0

            while not done:
                action, _ = custom_model.predict(state, deterministic=True)
                state, reward, done, _ = custom_env.step(action)
                episode_reward += reward

            custom_rewards.append(episode_reward)

        avg_custom_reward = sum(custom_rewards) / episodes
        print(f"Average reward with custom reward function: {avg_custom_reward}")

         

    Compare the baseline and modified agent’s performance. How did changing the reward function affect behavior?
    Modify the Model Architecture

    Now, experiment with a custom neural network architecture to see how model complexity affects performance.

    from stable_baselines3.common.torch_layers import BaseFeaturesExtractor
    import torch as th
    from torch import nn
    from stable_baselines3 import PPO
    from stable_baselines3.common.env_util import make_vec_env
    from stable_baselines3.common.utils import set_random_seed

    class CustomMLP(nn.Module):
        def __init__(self):
            super(CustomMLP, self).__init__()
            self.network = nn.Sequential(
                nn.Linear(4, 64), nn.ReLU(),
                nn.Linear(64, 64), nn.ReLU(),
                nn.Linear(64, 2)
            )

        def forward(self, x):
            return self.network(x)

    # Modify the policy architecture
    model = PPO("MlpPolicy", env, policy_kwargs=dict(activation_fn=th.nn.ReLU, net_arch=[128, 128]), verbose=1)

     

    Train the agent with the custom architecture and evaluate in the same way as the above section 2.
    Report and Analysis

    Submit a report that includes:
        Code files with each of the modifications.
        Recorded baseline and modified performance metrics.
        Reflections on how the custom reward function and neural network architecture affected the agent's performance.
