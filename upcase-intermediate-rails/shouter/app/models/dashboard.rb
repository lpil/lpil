class Dashboard
  def initialize(user)
    @user = user
  end

  def new_text_shout
    TextShout.new
  end

  def new_photo_shout
    PhotoShout.new
  end

  def timeline
    Timeline.new @user
  end
end
