# Coinkeper Helper
CLI application, improves existing [Coinkeeper](https://coinkeeper.me/) application. Helps being more mindful regarding finances.
Doesn't yet support the latest, third, version of Coinkeeper.

After run, connects to the Coinkeeper API and extract financial data to calculate:

1. Total amount available for spending today ignoring today's transactions.
2. Amount of available money for spending right at the moment.

Example output:
```bash
Total: 2223 Available: -3199
```

## Configuration
Configuration provided by environment variables:
1. `COINKEEPER_BUDGET` - Integer, how much you plan to spend in a month
2. `COINKEEPER_COOKIE` - String, cookie allowing querying `Coinkeeper` backend
3. `COINKEEPER_USER_ID` - String, user id allowing querying `Coinkeeper` backend

## Obtaining COINKEEPER_COOKIE and COINKEEPER_USER_ID

### COINKEEPER_COOKIE
The easiest way is to sniff `XHR` queries made by browser when working with the [web version of the Coinkeeper](https://coinkeeper.me)
![How to find out cookie](/cookie.png)

### COINKEEPER_USER_ID
The easiest way is to sniff `XHR` queries made by browser when working with the [web version of the Coinkeeper](https://coinkeeper.me)
![How to find out user id](/user_id.png)