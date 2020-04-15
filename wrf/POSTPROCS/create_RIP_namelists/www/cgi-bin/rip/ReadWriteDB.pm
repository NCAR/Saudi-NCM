package ReadWriteDB;

use GDBM_File;

$records="/model/atecuser/web_extra/form_data/records.dbx";
	
sub WriteData
{
	my($key,$value) = @_;
	tie(%DATA, GDBM_File, $records, GDBM_WRITER, 0) || print $!;
	$DATA{$key} = $value;
	untie(%DATA);
}

sub ReadData
{
	my($key) = @_;
	tie(%DATA, GDBM_File, $records, GDBM_READER, 0); 
        my($value) = $DATA{$key};
        untie(%DATA);	
	return($value);
}

sub GetData
{
	my($buffer) = @_;
	my($pair);
	my(%DEF);
	if($buffer)
	{
		@pairs = split(/&/,$buffer);
		foreach $pair (@pairs)
		{
			my($name,$value) = split(/=/,$pair);
			$value =~ s/%([0-9a-fA-F]{2})/pack("c",hex($1))/ge;
			$DEF{$name} = $value;
		}
		return %DEF;
																								 
	 }
	 return 0;
}

sub DeleteData
{
        my($key) = @_;
        tie(%DATA, GDBM_File, $records, GDBM_WRITER, 0); 
        my($value) = $DATA{$key};
        delete $DATA{$key};
        untie(%DATA);
}

1;
