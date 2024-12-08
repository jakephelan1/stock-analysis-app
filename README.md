# Stock Portfolio Analysis Project

Created by: Jake Phelan, Daniel Sachs, and Alan Zhao 

https://github.com/user-attachments/assets/d81dbac5-6c00-46f2-9404-5ec069b2afb5

## Project Overview
This project is a stock portfolio analysis tool built using OCaml and the Opium framework. It fetches financial data from the Alpha Vantage API, calculates financial ratios, and provides recommendations for portfolio optimization.

### Features
- Fetches real-time stock data from Alpha Vantage API
- Calculates financial ratios including liquidity, efficiency, leverage, and profitability
- Provides stock categorizations based on calculated ratios
- Generates recommendations for portfolio optimization
- Supports multiple stocks in a portfolio
- Includes a comprehensive test suite for reliability

### Tools Used
**OCaml:** Primary programming language

**Opium:** Web framework for the application

**Cohttp:** HTTP client for making API requests

**Yojson:** JSON parsing and manipulation

**OUnit2:** Unit testing framework

**Lwt:** Concurrent programming library

## Project Structure
- `main.ml`: Entry point of the application
- `api_client.ml`: Handles API requests to Alpha Vantage
- `portfolio.ml`: Manages the user's stock portfolio
- `ratios.ml`: Calculates financial ratios
- `stock_analysis.ml`: Analyzes stocks and generates scores
- `web_server.ml`: Handles web requests and responses

## Setup and Installation
To set up and run this project locally, follow these steps:

1. **Clone the Repository**

```bash
git clone https://github.com/jakephelan1/stock-analysis-app.git
cd stock-analysis-app
```

2. **Install Dependencies**
Ensure you have OCaml and OPAM installed. Then run:
```bash
opam install lwt_ssl lwt_ppx cohttp-lwt-unix opium yojson csv
```

3. **Build the Project**
```bash
dune build
```

4. Run Tests (Optional):
```bash
dune test
```

5. Start the Application
```bash
dune exec bin/main.exe
```

## Usage
Once the application is running:

1. Navigate to http://localhost:3000 in your web browser
2. Enter your stock portfolio information
3. Submit to receive analysis and recommendations
4. View detailed financial ratios and portfolio optimization suggestions.

Note: If you recieve an 'Internal Server Error' it is due to the API only allowing 25 requests per day.

## Testing
The project includes comprehensive unit tests covering:
- Financial ratio calculations
- API client functionality
- Portfolio management

## Note
This project is for educational purposes only and is not gauranteed to be 100% accurate.
