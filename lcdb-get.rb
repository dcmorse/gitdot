# db-get.rb downloads today's nightly backup to your home directory
# without the hassle of scrolling through a gazillion old backups in a
# GUI.
# 
# You need to have installed and configured the aws command line tool
# for this to work. See
# http://docs.aws.amazon.com/cli/latest/userguide/installing.html . It
# takes about 5 minutes.
#
# Invoke with the shell command `ruby ~/db-get.rb`

date = Time.new
filename = "launchcode_backup_#{date.strftime '%Y-%m-%d'}_01-13-01.sql"
cloud = "s3://launchcode-backup/mysql1/mysql/#{filename}"
local = "#{ENV['HOME']}/#{filename}"
cmd = "aws s3 cp #{cloud} #{local}"
puts "system('#{cmd}')"
if system(cmd)
  puts "wrote #{local}"
else
  puts "couldn't download database from '#{cloud}'"
end
