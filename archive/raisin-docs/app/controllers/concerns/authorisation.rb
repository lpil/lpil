module Authorisation
  def only_allow_signed_in_user
    redirect_to '/signin', notice: 'Please sign in' unless signed_in?
  end
  
  def block_access
    sign_out
    redirect_to '/signin', alert: 'Not enough permissions to view this page'
  end
end
