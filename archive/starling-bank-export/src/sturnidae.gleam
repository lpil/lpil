import gleam/http/request.{Request}
import gleam/http/response.{Response}
import gleam/option.{Option}
import gleam/result.{try}
import gleam/json
import gleam/dynamic as dyn

pub const v2 = "https://api.starlingbank.com/api/v2"

pub type StarlingApiResult(t) =
  Result(t, StarlingApiError)

pub type StarlingApiError {
  StarlingClientError(status_code: Int, messages: List(String))
  StarlingServerError(status_code: Int, message: String)
}

pub type Direction {
  /// Inbound transactions are money coming into the account.
  In
  /// Outbound transactions are money going out of the account.
  Out
}

pub fn direction_to_string(direction: Direction) -> String {
  case direction {
    In -> "IN"
    Out -> "OUT"
  }
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
    counter_party_type: Option(String),
    /// The unique identifier for the counter party. e.g. for MERCHANT this will
    /// be the merchant uid, for PAYEE this will be the payee uid.
    counter_party_uid: Option(String),
    /// The name of the counter party
    counter_party_name: String,
    total_fees: Option(Int),
    total_fee_amount: Option(CurrencyAndAmount),
    reference: Option(String),
    /// The country in which the transaction took place. ISO 3166-1 alpha-2
    /// country code.
    country: String,
    // The category of a transaction. e.g. GROCERIES
    spending_category: String,
    /// The user-provided transaction note
    user_note: Option(String),
  )
}

fn decode_currency_and_amount(
  d: dyn.Dynamic,
) -> Result(CurrencyAndAmount, dyn.DecodeErrors) {
  use currency <- try(dyn.field("currency", dyn.string)(d))
  use minor_units <- try(dyn.field("minorUnits", dyn.int)(d))
  Ok(CurrencyAndAmount(currency, minor_units))
}

fn decode_direction(d: dyn.Dynamic) -> Result(Direction, dyn.DecodeErrors) {
  use direction <- try(dyn.string(d))
  case direction {
    "IN" -> Ok(In)
    "OUT" -> Ok(Out)
    _ ->
      Error([dyn.DecodeError(expected: "Direction", found: "String", path: [])])
  }
}

fn decode_transaction_status(
  d: dyn.Dynamic,
) -> Result(TransactionStatus, dyn.DecodeErrors) {
  use status <- try(dyn.string(d))
  case status {
    "UPCOMING" -> Ok(Upcoming)
    "PENDING" -> Ok(Pending)
    "REVERSED" -> Ok(Reversed)
    "SETTLED" -> Ok(Settled)
    "DECLINED" -> Ok(Declined)
    "REFUNDED" -> Ok(Refunded)
    "RETRYING" -> Ok(Retrying)
    "ACCOUNT_CHECK" -> Ok(AccountCheck)
    _ ->
      Error([
        dyn.DecodeError(
          expected: "TransactionStatus",
          found: "String",
          path: [],
        ),
      ])
  }
}

pub fn decode_feed_item(d: dyn.Dynamic) -> Result(FeedItem, dyn.DecodeErrors) {
  let string_field = fn(field) { dyn.field(field, dyn.string)(d) }

  use feed_item_uid <- try(string_field("feedItemUid"))
  use category_uid <- try(string_field("categoryUid"))
  use amount <- try(dyn.field("amount", decode_currency_and_amount)(d))
  use source_amount <- try(dyn.field("sourceAmount", decode_currency_and_amount)(
    d,
  ))
  use updated_at <- try(string_field("updatedAt"))
  use direction <- try(dyn.field("direction", decode_direction)(d))
  use transaction_time <- try(string_field("transactionTime"))
  use source <- try(string_field("source"))
  use status <- try(dyn.field("status", decode_transaction_status)(d))
  use counter_party_type <- try(dyn.optional_field(
    "counterPartyType",
    dyn.string,
  )(d))
  use counter_party_uid <- try(dyn.optional_field("counterPartyUid", dyn.string)(
    d,
  ))
  use counter_party_name <- try(string_field("counterPartyName"))
  use total_fees <- try(dyn.optional_field("totalFees", dyn.int)(d))
  use total_fee_amount <- try(dyn.optional_field(
    "totalFeeAmount",
    decode_currency_and_amount,
  )(d))
  use reference <- try(dyn.optional_field("reference", dyn.string)(d))
  use country <- try(string_field("country"))
  use spending_category <- try(string_field("spendingCategory"))
  use user_note <- try(dyn.optional_field("userNote", dyn.string)(d))
  Ok(FeedItem(
    feed_item_uid,
    category_uid,
    amount,
    source_amount,
    updated_at,
    direction,
    transaction_time,
    source,
    status,
    counter_party_type,
    counter_party_uid,
    counter_party_name,
    total_fees,
    total_fee_amount,
    reference,
    country,
    spending_category,
    user_note,
  ))
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
  use _ <- try(check_status(response))
  let decoder = dyn.field("feedItems", dyn.list(decode_feed_item))
  decode_json_body(response, decoder)
}

fn check_status(response: Response(String)) -> StarlingApiResult(Nil) {
  let status = response.status
  case Nil {
    _ if status >= 200 && status < 300 -> {
      Ok(Nil)
    }

    _ if status >= 400 && status < 500 -> {
      // Client Error
      // TODO: decode error messages
      let messages = []
      Error(StarlingClientError(status_code: status, messages: messages))
    }

    _ -> {
      // Server error
      Error(StarlingServerError(status_code: status, message: response.body))
    }
  }
}

fn decode_json_body(
  response: Response(String),
  decoder: dyn.Decoder(t),
) -> StarlingApiResult(t) {
  case json.decode(response.body, decoder) {
    Ok(json) -> Ok(json)
    Error(_) -> {
      Error(StarlingServerError(
        status_code: response.status,
        message: "Invalid JSON returned by server",
      ))
    }
  }
}
