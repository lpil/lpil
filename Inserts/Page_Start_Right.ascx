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

<style type="text/css">
div#page_right {
  width: 160px;
  margin-left: 5px;
  font-size: 12px;
  position: relative;
  top: -12px;
  text-decoration:none;
}
#page_right > div {
  display: block;
  margin: 10px 3px;
  padding: 10px;
}
#quick_links  {
  background: url(Custom/Themes/sjp_2014_06/CSS/Images/blue_box_repeater.gif) repeat-x scroll left bottom #E6F2F6;
}
#page_right > div > p {
  margin: 0 0 10px 0;
}
#quick_links > p:after {
  position: relative;
  top: 1px;
  content: url(Custom/Themes/sjp_2014_06/CSS/Images/breadcrumb_arrow.png);
}
#quick_links > h2 {
  margin: 5px 0 10px 0;
}
#basketIcon {
  height: 22px;
  width: 22px;
  background-repeat: no-repeat;
  padding-bottom: 5px;
}

/* Information tooltip style */
.tooltip {
  color: #182B49;
  cursor: help; text-decoration: none;
  position: relative;
  font-family: Arial, sans-serif;
  font-size: 14px; font-weight: normal;
  top: 6px;
}
.tooltip span {
  margin-left: -999em;
  position: absolute; left: 0em; top: 2em; z-index: 99;
  decoration:	
}
.tooltip:hover span {
  border-radius: 5px 5px; -moz-border-radius: 5px; -webkit-border-radius: 5px; 
  box-shadow: 5px 5px 5px rgba(0, 0, 0, 0.1); -webkit-box-shadow: 5px 5px rgba(0, 0, 0, 0.1); -moz-box-shadow: 5px 5px rgba(0, 0, 0, 0.1);
  font-family: Arial, sans-serif;
  position: absolute; left: 0em; top: 2em; z-index: 99;
  margin-left: 0; width: 125px; height: 250px;
  font-family: Arial, sans-serif; font-size: 13px; font-weight: normal;
  display:block;
}
.tooltip img {
  width: 22px;
  height: 22px;
  padding-right: 3px;
  text-decoration:none;
}
.custom { padding: 0.5em 0.8em 0.5em 0.5em; }
* html a:hover { background: transparent; }
.info { background: #dfeef3; border: 1px solid #182B49;	}
</style>

<div id="page_right">
  <div id="right_basket" class="group">
    <p>
      You have <asp:Label id="numCart" style="color:#ad9f96;" CssClass="numCart" runat="server"/> <asp:Label id="ItemOrItems" runat="server"/> in your basket.
    </p>
    <div><table>
  <tbody><tr>
    <td><a id="ctl17_ctl01_ctl00_btnViewCart" class="siteButton" name="btnViewCart" href="javascript:__doPostBack('ctl17$ctl01$ctl00$btnViewCart','')">View Your Basket</a></td>
    <td><img id="basketIcon" src="http://www.sjpconnect.co.uk/Custom/Themes/sjp_2014_06/Inserts/Images/sjpbasket.png" alt="basket">	</td></tr></tbody></table></div>
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
    <h2>Track Your Order<img src="http://www.sjpconnect.co.uk/Custom/Themes/sjp_2014_06/Inserts/Images/sjpdeliverytruck.png" alt="truck"></h2>
        <a class="tooltip" href="#"><img src="http://www.sjpconnect.co.uk/Custom/Themes/sjp_2014_06/Inserts/Images/Info.png" alt="Information"/><span class="custom info">No record found may be due to one of the following reasons:</span>
    <input type="text" value="" name="order_ref" id="track_order_input"
           placeholder="Order Ref"
           onKeydown="javascript:if (event.which == 13) {
             return getMailInfo(
               document.getElementById('track_order_input').value);}">
    <a onclick="javascript:getMailInfo(
      document.getElementById('track_order_input'
      ).value)"
      href="javascript:void(0)" class="siteButton">Go</a>
    <div id="mail_result">
    </div>
  </div>
</div>
