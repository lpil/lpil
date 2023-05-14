import gleam/http/request.{Request}
import gleam/http/response.{Response}
import gleam/option.{Option}

pub const v2 = "https://api.starlingbank.com/api/v2"

pub type StarlingApiResult(t) =
  Result(t, StarlingApiError)

pub type StarlingApiError {
  StarlingClientError(status_code: Int, errors: List(String))
  StarlingServerError(status_code: Int, message: String)
}

pub type Direction {
  /// Inbound transactions are money coming into the account.
  In
  /// Outbound transactions are money going out of the account.
  Out
}

pub type CurrencyAndAmount {
  CurrencyAndAmount(
    /// The currency of the amount. e.g. GBP.
    currency: String,
    /// The amount in minor units. e.g. pence in GBP.
    minor_units: Int,
  )
}

pub type TransactionStatus {
  Upcoming
  Pending
  Reversed
  Settled
  Declined
  Refunded
  Retrying
  AccountCheck
}

pub fn transaction_status_to_string(status: TransactionStatus) -> String {
  case status {
    Upcoming -> "UPCOMING"
    Pending -> "PENDING"
    Reversed -> "REVERSED"
    Settled -> "SETTLED"
    Declined -> "DECLINED"
    Refunded -> "REFUNDED"
    Retrying -> "RETRYING"
    AccountCheck -> "ACCOUNT_CHECK"
  }
}

pub fn string_to_transaction_status(
  status: String,
) -> Result(TransactionStatus, Nil) {
  case status {
    "UPCOMING" -> Ok(Upcoming)
    "PENDING" -> Ok(Pending)
    "REVERSED" -> Ok(Reversed)
    "SETTLED" -> Ok(Settled)
    "DECLINED" -> Ok(Declined)
    "REFUNDED" -> Ok(Refunded)
    "RETRYING" -> Ok(Retrying)
    "ACCOUNT_CHECK" -> Ok(AccountCheck)
    _ -> Error(Nil)
  }
}

/// An item from the account holders's transaction feed.
pub type FeedItem {
  FeedItem(
    /// Unique identifier for this item.
    feed_item_uid: String,
    /// The category on which the transaction happened.
    category_uid: String,
    amount: CurrencyAndAmount,
    source_amount: CurrencyAndAmount,
    /// The time the transaction was last updated at.
    updated_at: String,
    /// The direction of the transaction.
    direction: Direction,
    /// The time of the transaction.
    transaction_time: String,
    /// The source of the transaction. e.g. MASTER_CARD.
    source: String,
    /// The status of a transaction.
    status: TransactionStatus,
    /// The type of counter party for a transaction. e.g. MERCHANT
    counter_party_type: String,
    /// The unique identifier for the counter party. e.g. for MERCHANT this will
    /// be the merchant uid, for PAYEE this will be the payee uid.
    counter_party_uid: String,
    /// The name of the counter party
    counter_party_name: String,
    total_fees: Option(Int),
    total_fee_amount: Option(CurrencyAndAmount),
    reference: String,
    /// The country in which the transaction took place. ISO 3166-1 alpha-2
    /// country code.
    country: String,
    // The category of a transaction. e.g. GROCERIES
    spending_category: String,
    /// The user-provided transaction note
    user_note: Option(String),
  )
}

/// Create a request for the
/// `/api/v2/feed/account/{accountUid}/category/{categoryUid}` endpoint.
///
/// 🔒 Security: Requires the transaction:read OAuth scope
///
/// Categories are subdivisions of an account. The default category holds the
/// main balance and transactions. Savings goals and spending spaces are
/// examples of other categories. You can use `/api/v2/accounts` to get the
/// default category UID for an account.
///
pub fn get_feed_items_request(
  pat pat: String,
  account_uid account: String,
  category_uid category: String,
  changes_since limit: String,
) -> Request(String) {
  let url = v2 <> "/feed/account/" <> account <> "/category/" <> category
  let url = url <> "?changesSince=" <> limit
  let assert Ok(request) = request.to(url)
  request
  |> request.set_header("accept", "application/json")
  |> request.set_header("authorization", "Bearer " <> pat)
}

/// Decode a response from the
/// `/api/v2/feed/account/{accountUid}/category/{categoryUid}` endpoint.
///
/// See the `get_feed_items_request` function for creating a request to this endpoint.
///
pub fn get_feed_items_response(
  response: Response(String),
) -> StarlingApiResult(List(FeedItem)) {
  todo
}
