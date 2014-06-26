def every(loop_length, error_pause_length = 120)
  Thread.new do
    loop do
      begin
        yield
        sleep loop_length
      rescue StandardError, ScriptError => e
        $logger.error { "'every' loop => #{e}\n#{e.backtrace.join "\n-> "}" }
        sleep error_pause_length
      end
    end
  end
end
