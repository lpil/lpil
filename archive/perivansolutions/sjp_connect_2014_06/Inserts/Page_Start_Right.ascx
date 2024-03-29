<%@ Control Language="c#" AutoEventWireup="True" %>
<%@ Register TagPrefix="PFWeb" Namespace="PFWeb" Assembly="PFWeb" %>
<script language="C#" runat="server">
string GetUserID(Pageflex.Ur.Storefront.Data.StorefrontAPI isini) {
  return isini.GetValue("SystemProperty", "LoggedOnUserID", null);
}
int GetShoppingCartCount() {
  Pageflex.Ur.Storefront.Data.StorefrontAPI isini =
    new Pageflex.Ur.Storefront.Data.StorefrontAPI();
  string userID = GetUserID(isini);
  string[] docsInCart = isini.GetListValue("UserListProperty", "DocumentsInShoppingCart", userID);
  return (docsInCart != null) ? docsInCart.Length : 0 ;
}
void Page_Load(object sender, EventArgs e) {
  if (!Page.IsPostBack) {
    int shoppingCount = GetShoppingCartCount();
    numCart.Text = shoppingCount.ToString();
    ItemOrItems.Text = (shoppingCount == 1) ? "item" : "items";
  }
}
</script>
<div id="page_right">
  <div id="right_basket" class="group">
    <p>
    You have <asp:Label id="numCart" style="color:#ad9f96;" CssClass="numCart" runat="server"/> <asp:Label id="ItemOrItems" runat="server"/> in your basket.
    </p>
    <table>
      <tbody>
      <tr>
        <td>
          <a id="ctl17_ctl01_ctl00_btnViewCart"
            name="btnViewCart" class="siteButton"
            href="javascript:__doPostBack('ctl17$ctl01$ctl00$btnViewCart','')">
            View Your Basket
          </a>
        </td>
        <td>
          <img src="Custom/Themes/sjp_2014_06/Inserts/Images/sjpbasket.png"
          alt="basket" id="basketIcon">
        </td>
      </tr>
      </tbody>
    </table>
  </div>

  <div id="quick_links">
    <h2>Quick Links</h2>
    <p>
      <a href="http://www.sjpconnect.co.uk/sjp_pdfs/Track%20Your%20deliveries.pdf">
        Delivery Tracking Guide
      </a> (PDF)
    </p>
    <p>
      <a href="http://www.sjpconnect.co.uk/sjp_pdfs/Guide%20to%20Investor%20Magazine.pdf">
        Investor Guide
      </a> (PDF)
    </p>
    <p>
      <a href="http://www.sjpconnect.co.uk/sjp_pdfs/Approvals%20User%20Guide.pdf">
        Stationery Approvals Guide
      </a> (PDF)
    </p>
  </div>

  <div id="track_order_div">
    <h2>
      Track Your Order
      <img src="Custom/Themes/sjp_2014_06/Inserts/Images/sjpdeliverytruck.png"
      alt="truck">
    </h2>
    <input type="text" value="" name="order_ref" id="track_order_input"
    placeholder="Order Ref"
    onKeydown="javascript:if (event.which == 13) {
    return getMailInfo( document.getElementById('track_order_input').value);}">

    <a onclick="javascript:getMailInfo(
      document.getElementById('track_order_input'
      ).value)"
      href="javascript:void(0)" class="siteButton">Go</a>
    <div id="mail_result">
    </div>
  </div>

  <!-- Popup search in category section -->
  <div id="search_in_cat_popup">
    <h2>Search in category</h2>
    <a href='javascript:searchInCat();void(0)' class='siteButton'>Next</a>
    <a href='javascript:slideSearchPopup(90,0);void(0);'
       id='close_search_in_cat_popup'>[x]</a>
  </div>
</div>
